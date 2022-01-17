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
    path::{Path, PathBuf},
    process::{Command, Stdio},
    sync::{atomic::AtomicBool, Arc},
};

use buildkit_llb::prelude::*;
use signal_hook::{
    consts::{SIGCHLD, SIGINT, SIGTERM},
    iterator::{exfiltrator::SignalOnly, SignalsInfo},
};

use crate::{
    imagegen::{self, BuildPlan},
    logic::Literal,
    modusfile::Modusfile,
};

use colored::Colorize;
use rand::{
    distributions::{Distribution, Uniform},
    Rng,
};
use std::io::Write;

use thiserror::Error;

pub const FRONTEND_IMAGE: &str = concat!("ghcr.io/modus-continens/modus-buildkit-frontend:", env!("GIT_SHA"));

#[derive(Error, Debug)]
pub enum BuildError {
    #[error("Could not get current working directory")]
    CwdError(#[source] std::io::Error),
    #[error("Could not enter context directory: {0}")]
    EnterContextDir(#[source] std::io::Error),
    #[error("Could not open a temporary file under the current directory: {0}. This is required to invoke docker build correctly.")]
    UnableToCreateTempFile(#[source] std::io::Error),
    #[error("Unable to run docker build.")]
    UnableToRunDockerBuild(#[source] std::io::Error),
    #[error("docker build exited with code {0}.")]
    DockerBuildFailed(i32),
    #[error("{0} contains invalid utf-8.")]
    FileHasInvalidUtf8(String),
    #[error("{0}")]
    IOError(
        #[from]
        #[source]
        std::io::Error,
    ),
    #[error("Interrupted by user.")]
    Interrupted,
}

use BuildError::*;

fn invoke_buildkit(
    dockerfile: &AutoDeleteTmpFilename,
    tag: Option<String>,
    target: Option<String>,
    has_dockerignore: bool,
    iidfile: Option<&str>,
    verbose: bool,
    signals: &mut SignalsInfo<SignalOnly>,
) -> Result<(), BuildError> {
    let mut args = Vec::new();
    args.push("build".to_string());
    args.push(".".to_string());
    args.push("-f".to_string());
    args.push(dockerfile.name().to_string());
    if let Some(tag) = tag {
        args.push("-t".to_string());
        args.push(tag);
    }
    let quiet = target.is_some();
    if let Some(target) = target {
        args.push("--target".to_string());
        args.push(target);
    }
    if quiet {
        args.push("--quiet".to_string());
    }
    args.push("--build-arg".to_string());
    if has_dockerignore {
        args.push("has_dockerignore=true".to_string());
    } else {
        args.push("has_dockerignore=false".to_string());
    }
    if let Some(iidfile) = iidfile {
        args.push("--iidfile".to_string());
        args.push(iidfile.to_string());
    }
    if verbose {
        args.push("--progress=plain".to_string());
    }
    let mut cmd = Command::new("docker")
        .args(args)
        .stdin(Stdio::null())
        .stdout(if quiet {
            Stdio::null()
        } else {
            Stdio::inherit()
        })
        .stderr(Stdio::inherit())
        .env("DOCKER_BUILDKIT", "1")
        .spawn()
        .map_err(|e| UnableToRunDockerBuild(e))?;
    let exit_status = loop {
        let mut has_sigchild = false;
        let mut has_term = false;
        for sig in signals.wait() {
            if sig == SIGCHLD {
                has_sigchild = true;
            } else if sig == SIGINT || sig == SIGTERM {
                has_term = true;
            }
        }
        if has_term {
            std::thread::sleep(std::time::Duration::from_millis(200)); // Wait for child to terminate first. This gives better terminal output.
            return Err(Interrupted);
        }
        if has_sigchild {
            let wait_res = cmd.try_wait().map_err(|e| IOError(e))?;
            if let Some(wait_res) = wait_res {
                break wait_res;
            }
        }
    };
    if !exit_status.success() {
        return Err(DockerBuildFailed(exit_status.code().unwrap_or(-1)));
    }
    Ok(())
}

/// A holder for a file name that deletes the file when dropped.
struct AutoDeleteTmpFilename(String);
pub const TMP_PREFIX: &str = "buildkit_temp_";
pub const TMP_PREFIX_IGNORE_PATTERN: &str = "buildkit_temp_*";

pub fn gen_tmp_filename() -> String {
    const RANDOM_LEN: usize = 15;
    let mut rng = rand::thread_rng();
    let letters = Uniform::new_inclusive('a', 'z');
    let uletters = Uniform::new_inclusive('A', 'Z');
    let mut name = String::with_capacity(TMP_PREFIX.len() + RANDOM_LEN);
    name.push_str(TMP_PREFIX);
    for _ in 0..RANDOM_LEN {
        if rng.gen_bool(0.5) {
            name.push(letters.sample(&mut rng));
        } else {
            name.push(uletters.sample(&mut rng));
        }
    }
    name
}

impl AutoDeleteTmpFilename {
    /// Generate a name, but does not create the file.
    fn gen(suffix: &str) -> Self {
        let mut name = gen_tmp_filename();
        name.push_str(suffix);
        Self(name)
    }

    fn name(&self) -> &str {
        &self.0
    }
}

impl Drop for AutoDeleteTmpFilename {
    fn drop(&mut self) {
        if let Err(e) = std::fs::remove_file(self.name()) {
            if e.kind() != std::io::ErrorKind::NotFound {
                eprintln!(
                    "Warning: unable to remove temporary file {}: {}",
                    self.name(),
                    e
                );
            }
        }
    }
}

fn write_tmp_dockerfile(content: &str) -> Result<AutoDeleteTmpFilename, std::io::Error> {
    let tmp_file = AutoDeleteTmpFilename::gen(".Dockerfile");
    let mut f = OpenOptions::new()
        .create_new(true)
        .write(true)
        .open(tmp_file.name())?;
    f.write_all(content.as_bytes())?;
    Ok(tmp_file)
}

struct RestoreCwd(PathBuf);
impl Drop for RestoreCwd {
    fn drop(&mut self) {
        if let Err(e) = std::env::set_current_dir(&self.0) {
            eprintln!(
                "Warning: unable to restore current directory to {}: {}",
                self.0.display(),
                e
            );
        }
    }
}

/// Returns the image IDs on success, following the order in build_plan.outputs.
pub fn build<P: AsRef<Path>>(
    build_plan: &BuildPlan,
    context: P,
    verbose: bool,
    frontend_image: &str
) -> Result<Vec<String>, BuildError> {
    let mut signals = SignalsInfo::with_exfiltrator(&[SIGINT, SIGTERM, SIGCHLD], SignalOnly)
        .expect("Failed to create signal handler.");

    fn check_terminate(signals: &mut SignalsInfo<SignalOnly>) -> bool {
        signals.pending().any(|s| s == SIGINT || s == SIGTERM)
    }
    if check_terminate(&mut signals) {
        return Err(Interrupted);
    }
    let previous_cwd = PathBuf::from(".").canonicalize().map_err(CwdError)?;
    std::env::set_current_dir(context).map_err(EnterContextDir)?;
    let _restore_cwd = RestoreCwd(previous_cwd);
    let has_dockerignore = check_dockerignore()?;
    let mut content = String::new();
    content.push_str("#syntax=");
    content.push_str(frontend_image);
    content.push('\n');
    content.push_str(&serde_json::to_string(build_plan).expect("Unable to serialize build plan"));
    if check_terminate(&mut signals) {
        return Err(Interrupted);
    }
    let dockerfile = write_tmp_dockerfile(&content).map_err(UnableToCreateTempFile)?;
    match build_plan.outputs.len() {
        0 => unreachable!(), // not possible because if there is no solution to the initial query, there will be an SLD failure.
        1 => {
            eprintln!("{}", "Running docker build...".blue());
            let iidfile = AutoDeleteTmpFilename::gen(".iid");
            invoke_buildkit(
                &dockerfile,
                None,
                None,
                has_dockerignore,
                Some(iidfile.name()),
                verbose,
                &mut signals,
            )?;
            if check_terminate(&mut signals) {
                return Err(Interrupted);
            }
            let iid = std::fs::read_to_string(iidfile.name())?;
            Ok(vec![iid])
        }
        nb_outputs => {
            eprintln!("{}", "Running docker build...".blue());
            if check_terminate(&mut signals) {
                return Err(Interrupted);
            }
            invoke_buildkit(
                &dockerfile,
                None,
                None,
                has_dockerignore,
                None,
                verbose,
                &mut signals,
            )?;
            let mut res = Vec::with_capacity(nb_outputs);
            let stderr = std::io::stderr();
            // TODO: check isatty
            let mut stderr = stderr.lock();
            write!(
                stderr,
                "\x1b[1A\x1b[2K\r=== Build success, exporting individual images ===\n"
            )?;
            for i in 0..nb_outputs {
                let target_str = format!("{}", i);
                write!(
                    stderr,
                    "{}",
                    format!(
                        "Exporting {}/{}: {}",
                        i + 1,
                        nb_outputs,
                        build_plan.outputs[i]
                            .source_literal
                            .as_ref()
                            .expect("Expected source_literal to present in build plan")
                            .to_string()
                    )
                    .blue()
                )?;
                stderr.flush()?;
                let iidfile = AutoDeleteTmpFilename::gen(".iid");
                invoke_buildkit(
                    &dockerfile,
                    None,
                    Some(target_str),
                    has_dockerignore,
                    Some(iidfile.name()),
                    verbose,
                    &mut signals,
                )?;
                let iid = std::fs::read_to_string(iidfile.name())?;
                write!(stderr, "{}", format!(" -> {}\n", &iid).blue())?;
                stderr.flush()?;
                res.push(iid);
            }
            Ok(res)
        }
    }
}

pub fn check_dockerignore() -> Result<bool, BuildError> {
    match std::fs::read(".dockerignore") {
        Ok(content) => {
            if std::str::from_utf8(&content).is_err() {
                Err(FileHasInvalidUtf8(".dockerignore".to_string()))
            } else {
                Ok(true)
            }
        }
        Err(e) => {
            if e.kind() != std::io::ErrorKind::NotFound {
                Err(e.into())
            } else {
                Ok(false)
            }
        }
    }
}
