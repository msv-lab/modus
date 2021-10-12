// Copyright 2021 Sergey Mechtaev

// This file is part of Modus.

// Modus is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// Modus is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Modus.  If not, see <https://www.gnu.org/licenses/>.

mod dockerfile;
mod logic;
mod wellformed;
mod modusfile;
mod unification;
mod sld;
mod transpiler;

#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate fp_core;

use clap::{
    crate_version,
    App,
    Arg
};
use std::fs;

use dockerfile::ResolvedDockerfile;
use modusfile::Modusfile;


fn main() {
    let matches = App::new("modus")
        .version(crate_version!())
        .about("Datalog-based container build system")
        .subcommand(App::new("transpile")
                    .arg(Arg::with_name("FILE")
                         .required(true)
                         .help("Sets the input Modusfile")
                         .index(1))
                    .arg(Arg::with_name("QUERY")
                         .required(true)
                         .help("Specifies the build target(s)")
                         .index(2))
                    .arg(Arg::with_name("proof")
                         .short("p")
                         .long("proof")
                         .help("Prints the proof tree of the target image"))
                    .arg(Arg::with_name("output")
                         .short("o")
                         .value_name("FILE")
                         .long("output")
                         .help("Sets the output Dockerfile")
                         .takes_value(true)))
        .get_matches();

    let input_file = matches.value_of("FILE").unwrap();
    let query: modusfile::Literal =
        matches.value_of("QUERY").map(|s| s.parse().unwrap()).unwrap();

    let file_content = fs::read_to_string(input_file).unwrap();
    let mf: Modusfile = file_content.parse().unwrap();

    let df: ResolvedDockerfile = transpiler::transpile(mf, query);

    println!("{}", df);
}
