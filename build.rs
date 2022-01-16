use std::{process::{Command, Stdio}};


fn main() -> Result<(), String> {
    let cmd = Command::new("git")
        .stdin(Stdio::null())
        .stderr(Stdio::inherit())
        .stdout(Stdio::piped())
        .args(&["rev-parse", "HEAD"])
        .output().map_err(|e| format!("Unable to run git: {}", e))?;
    if !cmd.status.success() {
      return Err(format!("git rev-parse failed with {}", cmd.status));
    }
    let sha = std::str::from_utf8(&cmd.stdout).map_err(|e| format!("git output: {}", e))?.trim();
    let short = &sha[0..10];
    println!("cargo:rustc-env=GIT_SHA={}", short);
    Ok(())
}
