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

mod common_parsers;
mod values;
mod dockerfile;
mod datalog;
mod modusfile;
mod transpiler;


#[macro_use]
extern crate fp_core;

use clap::{
    crate_version,
    App,
    Arg
};
use std::fs;

use dockerfile::{ Dockerfile, ResolvedParent };
use modusfile::{ Modusfile, ModusLiteral };
use datalog::Literal;


fn main() {
    let matches = App::new("modus-transpile")
        .version(crate_version!())
        .about("Converts Modusfile to Dockerfile")
        .arg(Arg::with_name("FILE")
            .required(true)
            .help("Sets the input Modusfile")
            .index(1))
        .arg(Arg::with_name("query")
            .short("q")
            .long("query")
            .value_name("TARGET")
            .help("Specifies the target image")
            .takes_value(true))
        .arg(Arg::with_name("proof")
            .short("p")
            .long("proof")
            .help("Prints the proof tree of the target image"))
        .arg(Arg::with_name("output")
            .short("o")
            .value_name("FILE")
            .long("output")
            .help("Sets the output Dockerfile")
            .takes_value(true))
        .get_matches();

        let input_file = matches.value_of("FILE").unwrap();
    let content = fs::read_to_string(input_file).unwrap();
    let mf: Modusfile = content.parse().unwrap();
    let query: Option<ModusLiteral> = matches.value_of("query").map(|s| s.parse().unwrap());
    let df: Dockerfile<ResolvedParent> = transpiler::transpile(mf, query);
    println!("{}", df);
}
