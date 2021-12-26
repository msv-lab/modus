//! The helper buildkit front-end mentioned in `buildkit.rs`.
#![allow(dead_code)]
// (otherwise there will be a lot of warnings for functions that are only used in the main binary.)

mod buildkit;
mod builtin;
mod dockerfile;
mod imagegen;
mod logic;
mod modusfile;
mod sld;
mod translate;
mod transpiler;
mod unification;
mod wellformed;

mod buildkit_llb_types;
use buildkit_llb_types::OwnedOutput;

use std::{collections::HashMap, path::PathBuf, sync::Arc};

use buildkit_frontend::{
    oci::{Architecture, ImageConfig, ImageSpecification, OperatingSystem},
    run_frontend, Bridge, Frontend, FrontendOutput,
};
use buildkit_llb::prelude::*;

use async_trait::async_trait;

use imagegen::{BuildNode, BuildPlan, NodeId};

#[macro_use]
extern crate serde;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    run_frontend(TheFrontend).await?;
    Ok(())
}

struct TheFrontend;

#[derive(Deserialize)]
struct FrontendOptions {
    filename: String,
    #[serde(flatten)]
    others: HashMap<String, serde_json::Value>,
}

#[async_trait]
impl Frontend<FrontendOptions> for TheFrontend {
    async fn run(
        self,
        bridge: Bridge,
        options: FrontendOptions,
    ) -> Result<FrontendOutput, failure::Error> {
        let build_plan = fetch_input(&bridge, &options).await;
        let outputs = handle_build_plan(&bridge, &build_plan).await;
        let final_output;
        if outputs.len() == 1 {
            final_output = outputs.into_iter().next().unwrap();
        } else {
            let alpine = Source::image("alpine")
                .custom_name("Getting an alpine image as a stub for the final image")
                .ref_counted();
            let (_, alpine_config) = bridge
                .resolve_image_config(&alpine, Some("alpine (stub) :: resolve"))
                .await?;
            let mut command = Command::run("true").cwd("/").mount(Mount::Layer(
                OutputIdx(0),
                SingleOwnedOutput::output(&alpine),
                "/",
            ));
            let mut idx = 1usize;
            for o in &outputs {
                command = command.mount(Mount::Layer(
                    OutputIdx(idx as u32),
                    o.0.output(),
                    format!("/_{}", idx),
                ));
                idx += 1;
            }
            command = command.custom_name("Finishing multiple output images");
            final_output = (
                OwnedOutput::from_command(command.ref_counted(), 0),
                Arc::new(alpine_config),
            );
        }
        let solved = bridge
            .solve(Terminal::with(final_output.0.output()))
            .await
            .expect("Unable to solve");
        Ok(FrontendOutput::with_spec_and_ref(
            (*final_output.1).clone(),
            solved,
        ))
    }
}

async fn read_local_file(bridge: &Bridge, filename: &str) -> Vec<u8> {
    let mut local_source = Source::local("context").custom_name(format!("Reading {}", filename));
    local_source = local_source.add_include_pattern(filename);
    let local_output = local_source.output();
    let local_ref = bridge
        .solve(Terminal::with(local_output))
        .await
        .expect("Failed to get local context");
    let input = bridge
        .read_file(&local_ref, filename, None)
        .await
        .expect("Failed to read local file");
    input
}

async fn fetch_input(bridge: &Bridge, options: &FrontendOptions) -> BuildPlan {
    let input_filename = &options.filename;
    let input_file_bytes = read_local_file(bridge, input_filename).await;
    let input_file_content =
        std::str::from_utf8(&input_file_bytes[..]).expect("Expected input to be UTF8");
    let start = input_file_content.find('\n').expect("Invalid input") + 1;
    serde_json::from_slice(&input_file_bytes[start..]).expect("Invalid input")
}

async fn handle_build_plan(
    bridge: &Bridge,
    build_plan: &BuildPlan,
) -> Vec<(OwnedOutput, Arc<ImageSpecification>)> {
    let mut translated_nodes: Vec<Option<(OwnedOutput, Arc<ImageSpecification>)>> =
        Vec::with_capacity(build_plan.nodes.len());
    for _ in 0..build_plan.nodes.len() {
        // Need to push in a loop since type is not cloneable.
        translated_nodes.push(None);
    }

    fn get_parent_dir(parent_config: &ImageSpecification) -> PathBuf {
        parent_config
            .config
            .as_ref()
            .and_then(|x| x.working_dir.clone())
            .map(|x| {
                if !x.has_root() {
                    PathBuf::from("/").join(x)
                } else {
                    x
                }
            })
            .unwrap_or_else(|| PathBuf::from("/"))
    }
    fn empty_image_config() -> ImageConfig {
        ImageConfig {
            user: None,
            exposed_ports: None,
            env: None,
            entrypoint: None,
            cmd: None,
            volumes: None,
            working_dir: None,
            labels: None,
            stop_signal: None,
        }
    }

    for node_id in build_plan.topological_order().into_iter() {
        let node = &build_plan.nodes[node_id];
        use BuildNode::*;
        let new_node: (OwnedOutput, Arc<ImageSpecification>) = match node {
            From { image_ref } => {
                let img_s = Source::image(image_ref).custom_name(format!("from({:?})", image_ref));
                let log_name = format!("from({:?}) :: resolve image config", image_ref);
                let (_, resolved_config) = bridge
                    .resolve_image_config(&img_s, Some(&log_name))
                    .await
                    .expect("Resolution failed.");
                (img_s.ref_counted().into(), Arc::new(resolved_config))
            }
            Run {
                parent,
                command,
                cwd,
            } => {
                let parent = translated_nodes[*parent]
                    .as_ref()
                    .expect("Expected dependencies to already be built");
                let parent_config = parent.1.clone();
                let o = OwnedOutput::from_command(
                    Command::run("sh") // TDDO: use image shell config
                        .args(&["-c", &command[..]])
                        .custom_name(format!("run({:?})", command))
                        .cwd(get_parent_dir(&parent_config).join(cwd))
                        .mount(Mount::Layer(OutputIdx(0), parent.0.output(), "/"))
                        .ref_counted(),
                    0,
                );
                (o, parent_config)
            }
            CopyFromImage {
                parent,
                src_image,
                src_path,
                dst_path,
            } => todo!(),
            CopyFromLocal {
                parent,
                src_path,
                dst_path,
            } => todo!(),
            SetWorkdir {
                parent,
                new_workdir,
            } => {
                let parent = translated_nodes[*parent]
                    .as_ref()
                    .expect("Expected dependencies to already be built");
                let parent_config = &*parent.1;
                let parent_dir = get_parent_dir(parent_config);
                let mut new_config = parent_config.clone();
                if new_config.config.is_none() {
                    new_config.config = Some(empty_image_config());
                }
                new_config.config.as_mut().unwrap().working_dir =
                    Some(parent_dir.join(new_workdir));
                let new_config = Arc::new(new_config);
                (parent.0.clone(), new_config)
            }
            SetEntrypoint {
                parent,
                new_entrypoint,
            } => todo!(),
        };
        translated_nodes[node_id] = Some(new_node);
    }
    let mut outputs: Vec<(OwnedOutput, Arc<ImageSpecification>)> = Vec::new();
    for o in &build_plan.outputs {
        outputs.push(
            translated_nodes[o.node]
                .clone()
                .expect("Expected output to be built"),
        );
    }
    outputs
}

async fn resolve_image_spec(build_plan: BuildPlan, node_id: NodeId) -> ImageSpecification {
    unimplemented!()
}
