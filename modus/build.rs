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

use std::process::{Command, Stdio};

fn main() -> Result<(), String> {
    let cmd = Command::new("git")
        .stdin(Stdio::null())
        .stderr(Stdio::inherit())
        .stdout(Stdio::piped())
        .args(&["rev-parse", "HEAD"])
        .output()
        .map_err(|e| format!("Unable to run git: {}", e))?;
    if !cmd.status.success() {
        return Err(format!("git rev-parse failed with {}", cmd.status));
    }
    let sha = std::str::from_utf8(&cmd.stdout)
        .map_err(|e| format!("git output: {}", e))?
        .trim();
    let short = &sha[0..10];
    println!("cargo:rustc-env=GIT_SHA={}", short);
    println!("cargo:rerun-if-changed=../.git");
    Ok(())
}
