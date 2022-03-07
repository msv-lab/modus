// Modus, a language for building container images
// Copyright (C) 2022 University College London

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.

// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

//! The helper buildkit front-end mentioned in `buildkit.rs`.
#![allow(dead_code)]
// (otherwise there will be a lot of warnings for functions that are only used in the main binary.)

mod buildkit;
mod reporting;

use modus_lib::*;

mod buildkit_llb_types;
use buildkit_llb_types::OwnedOutput;

use std::{
    collections::{BTreeMap, HashMap},
    ffi::{OsStr, OsString},
    path::PathBuf,
    sync::Arc,
};

use buildkit_frontend::{
    oci::{ImageConfig, ImageSpecification},
    run_frontend, Bridge, Frontend, FrontendOutput,
};
use buildkit_llb::prelude::*;

use async_trait::async_trait;

use imagegen::{BuildNode, BuildPlan};

use crate::imagegen::{MergeNode, MergeOperation};

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
    target: Option<String>,
    has_dockerignore: bool,
    no_cache: bool,
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
        let mut outputs = handle_build_plan(&bridge, &options, &build_plan).await;
        let final_output;
        if outputs.len() == 1 {
            final_output = outputs.into_iter().next().unwrap();
        } else if options.target.is_some() && !options.target.as_ref().unwrap().is_empty() {
            let target_idx: usize = options
                .target
                .as_ref()
                .unwrap()
                .parse()
                .expect("Expected target to be an usize");
            final_output = outputs.swap_remove(target_idx);
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
    options: &FrontendOptions,
    build_plan: &BuildPlan,
) -> Vec<(OwnedOutput, Arc<ImageSpecification>)> {
    let mut translated_nodes: Vec<Option<(OwnedOutput, Arc<ImageSpecification>)>> =
        Vec::with_capacity(build_plan.nodes.len());
    for _ in 0..build_plan.nodes.len() {
        // Need to push in a loop since type is not cloneable.
        translated_nodes.push(None);
    }

    fn get_cwd_from_image_spec(image_spec: &ImageSpecification) -> PathBuf {
        image_spec
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
    fn scratch_spec() -> ImageSpecification {
        ImageSpecification {
            architecture: buildkit_frontend::oci::Architecture::Amd64, // TODO
            author: None,
            config: Some(empty_image_config()),
            created: None,
            history: None,
            os: buildkit_frontend::oci::OperatingSystem::Linux,
            rootfs: None,
        }
    }

    async fn get_local_source_for_copy(
        bridge: &Bridge,
        should_read_ignore_file: bool,
    ) -> OperationOutput<'static> {
        let mut source = Source::local("context").custom_name("Sending local context for copy");
        if should_read_ignore_file {
            let dockerignore_bytes = read_local_file(bridge, ".dockerignore").await;
            let dockerignore = std::str::from_utf8(&dockerignore_bytes)
                .expect("Expected .dockerignore to contain valid utf-8 content.");
            for line in dockerignore.lines() {
                source = source.add_exclude_pattern(line);
            }
        }
        source = source.add_exclude_pattern(buildkit::TMP_PREFIX_IGNORE_PATTERN);
        source.ref_counted().output()
    }

    let local_context = get_local_source_for_copy(bridge, options.has_dockerignore).await;

    for node_id in build_plan.topological_order().into_iter() {
        let node = &build_plan.nodes[node_id];
        use BuildNode::*;

        fn new_cmd(
            imgspec: &ImageSpecification,
            this_cwd: &str,
            parent: &OwnedOutput,
            frontend_options: &FrontendOptions,
        ) -> Command<'static> {
            let mut cmd = Command::run("sh"); // TDDO: use image shell config
            let user = imgspec
                .config
                .as_ref()
                .and_then(|x| x.user.as_ref().map(|x| &x[..]))
                .unwrap_or("0");
            cmd = cmd
                .cwd(get_cwd_from_image_spec(&imgspec).join(this_cwd))
                .user(user);
            let envs = imgspec.config.as_ref().and_then(|x| x.env.as_ref());
            if let Some(env_map) = envs {
                for (key, value) in env_map.iter() {
                    cmd = cmd.env(key, value);
                }
            }
            cmd = cmd.mount(Mount::Layer(OutputIdx(0), parent.output(), "/"));
            if frontend_options.no_cache {
                cmd = cmd.ignore_cache(true);
            }
            cmd
        }

        fn iter_hm_sorted<K: Ord, V>(hm: &HashMap<K, V>) -> Vec<(&K, &V)> {
            let mut v = hm.iter().collect::<Vec<_>>();
            v.sort_unstable_by_key(|(k, _)| *k);
            v
        }

        fn add_envs<'a>(mut cmd: Command<'a>, envs: &HashMap<String, String>) -> Command<'a> {
            for (k, v) in iter_hm_sorted(envs) {
                cmd = cmd.env(k, v);
            }
            cmd
        }

        let new_node: (OwnedOutput, Arc<ImageSpecification>) = match node {
            /*
                resolve_image_config will fail if we try to resolve an empty
                image (like that produced by FROM scratch). Therefore, we have a
                special case here for scratch images that don't try to resolve
                its spec.
            */
            FromScratch { scratch_ref } => {
                let img_s =
                    Source::image(scratch_ref.as_ref().unwrap()).custom_name("from(\"scratch\")");
                (img_s.ref_counted().into(), Arc::new(scratch_spec()))
            }
            From {
                image_ref,
                display_name,
            } => {
                let img_s =
                    Source::image(image_ref).custom_name(format!("from({:?})", display_name));
                let log_name = format!("from({:?}) :: resolve image config", display_name);
                let resolved_config =
                    match bridge.resolve_image_config(&img_s, Some(&log_name)).await {
                        Ok((_, x)) => x,
                        Err(e) => {
                            panic!("Failed to resolve image config: {:?}", e); // unreachable
                        }
                    };
                (img_s.ref_counted().into(), Arc::new(resolved_config))
            }
            Run {
                parent,
                command,
                cwd,
                additional_envs,
            } => {
                let parent = translated_nodes[*parent]
                    .as_ref()
                    .expect("Expected dependencies to already be built");
                let parent_config = parent.1.clone();
                let mut cmd = new_cmd(&*parent_config, &cwd[..], &parent.0, &options)
                    .args(&["-c", &command[..]])
                    .custom_name(format!("run({:?})", command));
                cmd = add_envs(cmd, additional_envs);
                let o = OwnedOutput::from_command(cmd.ref_counted(), 0);
                (o, parent_config)
            }
            CopyFromImage {
                parent,
                src_image,
                src_path: raw_src_path,
                dst_path: raw_dst_path,
            } => {
                let parent = translated_nodes[*parent].as_ref().unwrap();
                let src_image = translated_nodes[*src_image].as_ref().unwrap();
                let src_cwd = get_cwd_from_image_spec(&src_image.1);
                let src_path = src_cwd.join(raw_src_path);
                let dst_path = get_cwd_from_image_spec(&parent.1).join(raw_dst_path);
                let o = FileSystem::copy()
                    .from(LayerPath::Other(src_image.0.output(), src_path))
                    .to(OutputIdx(0), LayerPath::Other(parent.0.output(), dst_path))
                    .create_path(true)
                    .recursive(true)
                    .into_operation()
                    .custom_name(format!(
                        "...::copy({:?}, {:?})",
                        &raw_src_path, &raw_dst_path
                    ))
                    .ref_counted();
                (o.into(), parent.1.clone())
            }
            CopyFromLocal {
                parent,
                src_path,
                dst_path: raw_dst_path,
            } => {
                let parent = translated_nodes[*parent].as_ref().unwrap();
                let dst_path = get_cwd_from_image_spec(&parent.1).join(raw_dst_path);
                let o = FileSystem::copy()
                    .from(LayerPath::Other(local_context.clone(), src_path))
                    .to(OutputIdx(0), LayerPath::Other(parent.0.output(), dst_path))
                    .create_path(true)
                    .recursive(true)
                    .into_operation()
                    .custom_name(format!("copy({:?}, {:?})", &src_path, &raw_dst_path))
                    .ref_counted();
                (o.into(), parent.1.clone())
            }
            SetWorkdir {
                parent,
                new_workdir,
            } => {
                let parent = translated_nodes[*parent]
                    .as_ref()
                    .expect("Expected dependencies to already be built");
                let parent_config = &*parent.1;
                let parent_dir = get_cwd_from_image_spec(parent_config);
                let mut new_config = parent_config.clone();
                new_config
                    .config
                    .get_or_insert_with(empty_image_config)
                    .working_dir = Some(parent_dir.join(new_workdir));
                let new_config = Arc::new(new_config);
                (parent.0.clone(), new_config)
            }
            SetEntrypoint {
                parent,
                new_entrypoint,
            } => {
                let (p_out, p_conf) = translated_nodes[*parent].clone().unwrap();
                let mut p_conf = (*p_conf).clone();
                p_conf
                    .config
                    .get_or_insert_with(empty_image_config)
                    .entrypoint = Some(new_entrypoint.to_owned());
                (p_out, Arc::new(p_conf))
            }
            SetLabel {
                parent,
                label,
                value,
            } => {
                let (p_out, p_conf) = translated_nodes[*parent].clone().unwrap();
                let mut p_conf = (*p_conf).clone();
                p_conf
                    .config
                    .get_or_insert_with(empty_image_config)
                    .labels
                    .get_or_insert_with(BTreeMap::new)
                    .insert(label.to_owned(), value.to_owned());
                (p_out, Arc::new(p_conf))
            }
            Merge(MergeNode { parent, operations }) => {
                let (p_out, p_conf) = translated_nodes[*parent].clone().unwrap();
                let mut cmd = new_cmd(&*p_conf, "", &p_out, &options);
                let mut name = Vec::new();
                let mut script = Vec::new();
                let image_cwd = get_cwd_from_image_spec(&*p_conf);
                debug_assert!(image_cwd.is_absolute());
                use shell_escape::escape;
                let mut mount_id = 0usize;
                fn mkdir_pf(path: &str, script: &mut Vec<String>) {
                    script.push(format!("(mkdir -p {} || true)", escape(path.into())));
                }
                fn cd(path: &str, script: &mut Vec<String>) {
                    mkdir_pf(path, script);
                    script.push(format!("echo cd {cd} && cd {cd}", cd = escape(path.into())));
                }
                fn cp_content(src: PathBuf, dst: &str, script: &mut Vec<String>) {
                    let src_str = src.to_str().unwrap();
                    let _s = src.join(".");
                    let src_plus_dot = _s.to_str().unwrap();
                    script.push(format!(
                        "echo COPY '->' {dst} && (if [ -d {src} ]; then cp -r {src_plus_dot} {dst}; else cp -r {src} {dst}; fi)",
                        src=escape(src_str.into()),
                        dst=escape(dst.into()),
                        src_plus_dot=escape(src_plus_dot.into())
                    ));
                }
                for op in operations {
                    match op {
                        MergeOperation::Run {
                            command,
                            cwd,
                            additional_envs,
                        } => {
                            let resolved_cwd = image_cwd.join(cwd);
                            let resolved_cwd = resolved_cwd.to_str().unwrap(); // TODO: report error if image cwd is not valid utf8.
                            cd(resolved_cwd, &mut script);
                            for (k, v) in iter_hm_sorted(additional_envs) {
                                script.push(format!(
                                    "export {}={}",
                                    escape(k.into()),
                                    escape(v.into())
                                ));
                            }
                            script.push(format!(
                                "echo {cmd} && sh -c {cmd}",
                                cmd = escape(command.into())
                            ));
                            name.push(format!("run({:?})::in_workdir({:?})", command, cwd));
                        }
                        MergeOperation::CopyFromImage {
                            src_image,
                            src_path,
                            dst_path,
                        } => {
                            let (src_opt, src_conf) = translated_nodes[*src_image].clone().unwrap();
                            let src_cwd = get_cwd_from_image_spec(&src_conf);
                            let src_path = src_cwd.join(src_path);
                            let dst_path = image_cwd.join(dst_path);

                            let mut mount_dir = OsString::from("/__buildkit_merge_mount_");
                            mount_dir.push(OsStr::new(&mount_id.to_string()));
                            mount_id += 1;
                            debug_assert!(src_path.is_absolute());
                            debug_assert!(dst_path.is_absolute());
                            mount_dir.push(&src_path);
                            let mount_dir = PathBuf::from(mount_dir);
                            cmd = cmd.mount(Mount::ReadOnlySelector(
                                src_opt.output(),
                                mount_dir.clone(),
                                src_path.clone(),
                            ));

                            if let Some(par) = dst_path.parent() {
                                mkdir_pf(par.to_str().unwrap(), &mut script);
                            }
                            cp_content(mount_dir, dst_path.to_str().unwrap(), &mut script);
                            name.push(format!("...::copy({:?}, {:?})", src_path, dst_path));
                        }
                        MergeOperation::CopyFromLocal { src_path, dst_path } => {
                            let mut mount_dir = OsString::from("/__buildkit_merge_mount_");
                            mount_dir.push(OsStr::new(&mount_id.to_string()));
                            mount_id += 1;
                            mount_dir.push("/");
                            debug_assert!(!src_path.starts_with("/"));
                            mount_dir.push(src_path);
                            let dst_path = image_cwd.join(dst_path);
                            debug_assert!(dst_path.is_absolute());
                            let mount_dir = PathBuf::from(mount_dir);
                            cmd = cmd.mount(Mount::ReadOnlySelector(
                                local_context.clone(),
                                mount_dir.clone(),
                                PathBuf::from(src_path),
                            ));

                            if let Some(par) = dst_path.parent() {
                                mkdir_pf(par.to_str().unwrap(), &mut script);
                            }
                            cp_content(mount_dir, dst_path.to_str().unwrap(), &mut script);
                            name.push(format!("copy({:?}, {:?})", src_path, dst_path));
                        }
                    }
                }
                cmd = cmd.args(&["-c", &script.join(" && ")]);
                cmd = cmd.custom_name(format!("merge: {}", name.join(" + ")));

                (OwnedOutput::from_command(cmd.ref_counted(), 0), p_conf)
            }
            SetEnv { parent, key, value } => {
                let (p_out, p_conf) = translated_nodes[*parent].clone().unwrap();
                let mut p_conf = (*p_conf).clone();
                p_conf
                    .config
                    .get_or_insert_with(empty_image_config)
                    .env
                    .get_or_insert_with(BTreeMap::new)
                    .insert(key.to_owned(), value.to_owned());
                (p_out, Arc::new(p_conf))
            }
            AppendEnvValue { parent, key, value } => {
                let (p_out, p_conf) = translated_nodes[*parent].clone().unwrap();
                let mut p_conf = (*p_conf).clone();
                p_conf
                    .config
                    .get_or_insert_with(empty_image_config)
                    .env
                    .get_or_insert_with(BTreeMap::new)
                    .entry(key.to_owned())
                    .or_insert_with(String::new)
                    .push_str(&value);
                (p_out, Arc::new(p_conf))
            }
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
