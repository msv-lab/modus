//! This module contains code which handles buildkit integration.
//!
//! To reduce friction, it is best that we do not require the user to install
//! buildkit themselves, which is actually another system service when running
//! on its own. Instead, we make use of the buildkit integration built into
//! `docker build` (search for `DOCKER_BUILDKIT` env) by creating our own little
//! "frontend" for buildkit.  In our case this would be a shim not intended to
//! be used directly by the user, but can be invoked simply by spawning `docker
//! build` in our code.
//!
//! When a user tries to build an image with our buildkit backend,
//! the following things will happen in order:
//!
//! 1. The outer modus program generates all required proofs and build plan for
//! the user's query, then encodes the build plan to json.
//!
//! 2. The outer program writes the json along with a special declaration line
//! `#syntax=...` to a temporary Dockerfile, and invokes `docker build`. The
//! `#syntax` declaration tells Docker to use our frontend instead of treating
//! it as a normal Dockerfile.
//!
//! 3. Our frontend (which runs inside a separate container created by docker
//! build) turns the json back into the build plan and generate all the required
//! buildkit calls to build all output images. If there are more than one output
//! images, it also generate a final empty image which depeneds on all the
//! output images user wants. This will effectively force buildkit to build
//! multiple output images in parallel, if that's what the user wants.
//!
//! 4. If there are multiple output images, invoke docker build again for each
//! output image, and instead of building the empty image, just build the
//! required output image, and also ignore all "no_cache". This should be
//! instant since everything is cached. We can also tell docker build to tag the
//! image with a name so that it can be referenced by the user.
//!
//! The easiest way to create this inner frontend is to build a separate binary.
//! Check out `buildkit_frontend.rs` for the main function of this inner
//! frontend.

use std::{
    fs::{File, OpenOptions},
    process::{Command, Stdio},
};

use buildkit_llb::prelude::*;

use crate::{
    imagegen::{self, BuildPlan},
    logic::Literal,
    modusfile::Modusfile,
};

use colored::Colorize;
use rand::distributions::{Distribution, Uniform};
use std::io::Write;

use thiserror::Error;

#[derive(Error, Debug)]
pub enum BuildError {
    #[error("Could not create a temporary file under the current directory.")]
    UnableToCreateTempFile(#[source] std::io::Error),
    #[error("Unable to run docker build.")]
    UnableToRunDockerBuild(#[source] std::io::Error),
    #[error("docker build exited with code {0}.")]
    DockerBuildFailed(i32),
}

use BuildError::*;

pub fn invoke_buildkit(build_plan: &BuildPlan, tag: Option<String>) -> Result<(), BuildError> {
    println!("{}", "Running docker build...".blue());
    let mut args = Vec::new();
    let mut content = String::new();
    content.push_str("#syntax=europe-docker.pkg.dev/maowtm/modus-test/modus-bk-frontend"); // TODO
    content.push('\n');
    content.push_str(&serde_json::to_string(&build_plan).expect("Unable to serialize build plan"));
    let tmp_file = write_tmp_dockerfile(&content).map_err(|e| UnableToCreateTempFile(e))?;
    args.push("build".to_string());
    args.push(".".to_string());
    args.push("-f".to_string());
    args.push(tmp_file.name().to_string());
    if let Some(tag) = tag {
        args.push("-t".to_string());
        args.push(tag);
    }
    let mut cmd = Command::new("docker")
        .args(args)
        .stdin(Stdio::null())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .env("DOCKER_BUILDKIT", "1")
        .spawn()
        .map_err(|e| UnableToRunDockerBuild(e))?;
    let exit_status = cmd.wait().expect("wait() failed");
    if !exit_status.success() {
        return Err(DockerBuildFailed(exit_status.code().unwrap_or(-1)));
    }
    Ok(())
}

struct TmpDockerfile(String);

impl TmpDockerfile {
    fn name(&self) -> &str {
        &self.0
    }
}

impl Drop for TmpDockerfile {
    fn drop(&mut self) {
        if let Err(e) = std::fs::remove_file(self.name()) {
            eprintln!(
                "Warning: unable to remove temporary file {}: {}",
                self.name(),
                e
            );
        }
    }
}

fn write_tmp_dockerfile(content: &str) -> Result<TmpDockerfile, std::io::Error> {
    let mut rng = rand::thread_rng();
    let letters = Uniform::new_inclusive('a', 'z');
    let mut name = String::with_capacity(40);
    name.push_str("buildkit_temp_");
    for _ in 0..10 {
        name.push(letters.sample(&mut rng));
    }
    name.push_str(".Dockerfile");
    let mut f = OpenOptions::new()
        .create_new(true)
        .write(true)
        .open(&name)?;
    f.write_all(content.as_bytes())?;
    Ok(TmpDockerfile(name))
}

pub fn build_modusfile(mf: Modusfile, query: Literal) {
    let build_plan = imagegen::plan_from_modusfile(mf, query);
    if let Err(e) = invoke_buildkit(&build_plan, None) {
        eprintln!("{}", e);
        std::process::exit(1);
    }
    // TODO: extract individual outputs
}
