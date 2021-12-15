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

use std::{collections::HashMap, path::PathBuf};

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
            let mut command = Command::run("true").mount(Mount::Layer(
                OutputIdx(0),
                SingleOwnedOutput::output(&alpine),
                "/",
            ));
            let mut idx = 1usize;
            for o in &outputs {
                command = command.mount(Mount::Layer(
                    OutputIdx(idx as u32),
                    o.output(),
                    format!("/_{}", idx),
                ));
                idx += 1;
            }
            command = command.custom_name("Finishing multiple output images");
            final_output = Box::new(MultiOwnedOutputWithIndex(command.ref_counted(), 0));
        }
        let solved = bridge
            .solve(Terminal::with(final_output.output()))
            .await
            .expect("Unable to solve");
        Ok(FrontendOutput::with_ref(solved))
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

type BoxedOutput = Box<dyn OwnedOutput + Send + Sync + 'static>;

pub trait OwnedOutput {
    fn output(&self) -> OperationOutput<'static>;
}

impl<T: SingleOwnedOutput<'static>> OwnedOutput for T {
    fn output(&self) -> OperationOutput<'static> {
        self.output()
    }
}

#[derive(Debug)]
struct MultiOwnedOutputWithIndex<T: MultiOwnedOutput<'static>>(T, u32);

impl<T: MultiOwnedOutput<'static>> OwnedOutput for MultiOwnedOutputWithIndex<T> {
    fn output(&self) -> OperationOutput<'static> {
        self.0.output(self.1)
    }
}

async fn handle_build_plan(bridge: &Bridge, build_plan: &BuildPlan) -> Vec<BoxedOutput> {
    let mut translated_nodes: Vec<Option<BoxedOutput>> = Vec::with_capacity(build_plan.nodes.len());
    for _ in 0..build_plan.nodes.len() {
        // Need to push in a loop since type is not cloneable.
        translated_nodes.push(None);
    }
    for node_id in build_plan.topological_order().into_iter() {
        let node = &build_plan.nodes[node_id];
        use BuildNode::*;
        let new_node: BoxedOutput = match node {
            From { image_ref } => Box::new(
                Source::image(image_ref)
                    .custom_name(format!("from({:?})", image_ref))
                    .ref_counted(),
            ),
            Run {
                parent,
                command,
                cwd,
            } => Box::new(MultiOwnedOutputWithIndex(
                Command::run("sh") // TDDO: use image shell config
                    .args(&["-c", &command[..]])
                    .custom_name(format!("run({:?})", command))
                    .cwd(PathBuf::from("/").join(cwd)) // TODO: properly join with cwd in image config
                    .mount(Mount::Layer(
                        OutputIdx(0),
                        translated_nodes[*parent]
                            .as_ref()
                            .expect("Expected dependencies to already be built")
                            .output(),
                        "/",
                    ))
                    .ref_counted(),
                0,
            )),
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
        };
        translated_nodes[node_id] = Some(new_node);
    }
    let mut outputs: Vec<BoxedOutput> = Vec::new();
    for o in &build_plan.outputs {
        outputs.push(
            translated_nodes[o.node]
                .take()
                .expect("Expected output to be built"),
        );
    }
    outputs
}

async fn resolve_image_spec(build_plan: BuildPlan, node_id: NodeId) -> ImageSpecification {
    unimplemented!()
}
