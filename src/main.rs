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

extern crate lazy_static;

#[macro_use]
extern crate fp_core;

use clap::{crate_version, App, Arg};
use codespan_reporting::{
    diagnostic,
    files::SimpleFile,
    term::{
        self,
        termcolor::{Color, ColorSpec, StandardStream, WriteColor},
    },
};
use colored::Colorize;
use std::io::Write;
use std::{fs, path::Path};

use dockerfile::ResolvedDockerfile;
use modusfile::Modusfile;

use crate::transpiler::prove_goal;

fn get_file(path: &Path) -> SimpleFile<&str, String> {
    let file_name: &str = path
        .file_name()
        .map(|os_str| os_str.to_str())
        .unwrap()
        .unwrap();
    let file_content: String = fs::read_to_string(path).unwrap();

    SimpleFile::new(file_name, file_content)
}

fn main() {
    let matches = App::new("modus")
        .version(crate_version!())
        .about("Datalog-based container build system")
        .subcommand(
            App::new("transpile")
                .arg(
                    Arg::with_name("FILE")
                        .required(true)
                        .help("Sets the input Modusfile")
                        .index(1),
                )
                .arg(
                    Arg::with_name("QUERY")
                        .required(true)
                        .help("Specifies the build target(s)")
                        .index(2),
                )
                .arg(
                    Arg::with_name("proof")
                        .short("p")
                        .long("proof")
                        .help("Prints the proof tree of the target image"),
                )
                .arg(
                    Arg::with_name("output")
                        .short("o")
                        .value_name("FILE")
                        .long("output")
                        .help("Sets the output Dockerfile")
                        .takes_value(true),
                ),
        )
        .subcommand(
            App::new("build")
                .arg(
                    Arg::with_name("FILE")
                        .required(true)
                        .help("Sets the input Modusfile")
                        .index(1),
                )
                .arg(
                    Arg::with_name("QUERY")
                        .required(true)
                        .help("Specifies the build target(s)")
                        .index(2),
                ),
        )
        .subcommand(
            App::new("proof")
                .arg(
                    Arg::with_name("FILE")
                        .required(true)
                        .help("Sets the input Modusfile")
                        .index(1),
                )
                .arg(
                    Arg::with_name("QUERY")
                        .help("Specifies the target to prove")
                        .index(2),
                ),
        )
        .get_matches();

    let writer = StandardStream::stderr(codespan_reporting::term::termcolor::ColorChoice::Auto);
    let config = codespan_reporting::term::Config::default();

    match matches.subcommand() {
        ("transpile", Some(sub)) => {
            let input_file = sub.value_of("FILE").unwrap();
            let file = get_file(Path::new(input_file));
            let query: logic::Literal = sub.value_of("QUERY").map(|s| s.parse().unwrap()).unwrap();

            let mf: Modusfile = file.source().parse().unwrap();

            let df_res = transpiler::transpile(mf, query);

            match df_res {
                Ok(df) => println!("{}", df),
                Err(e) => term::emit(&mut writer.lock(), &config, &file, &e)
                    .expect("Error when printing to stderr."),
            }
        }
        ("build", Some(sub)) => {
            let input_file = sub.value_of("FILE").unwrap();
            let file = get_file(Path::new(input_file));
            let query: logic::Literal = sub.value_of("QUERY").map(|s| s.parse().unwrap()).unwrap();

            let mf: Modusfile = match file.source().parse() {
                Ok(mf) => mf,
                Err(e) => {
                    eprintln!("{}", e);
                    std::process::exit(1);
                }
            };
            let build_plan = match imagegen::plan_from_modusfile(mf, query) {
                Ok(plan) => plan,
                Err(e) => {
                    term::emit(&mut writer.lock(), &config, &file, &e)
                        .expect("Error when printing to stderr.");
                    std::process::exit(1);
                }
            };
            match buildkit::build(&build_plan) {
                Err(e) => {
                    let mut w = writer.lock();
                    (move || -> std::io::Result<()> {
                        w.set_color(ColorSpec::new().set_fg(Some(Color::Red)).set_bold(true))?;
                        write!(w, "build error")?;
                        w.set_color(&ColorSpec::new())?;
                        write!(w, ": ")?;
                        w.set_color(ColorSpec::new().set_bold(true))?;
                        write!(w, "{}\n", e)?;
                        w.flush()?;
                        Ok(())
                    })()
                    .expect("Unable to write to stderr.");
                    std::process::exit(1);
                }
                Ok(_) => {}
            }
        }
        ("proof", Some(sub)) => {
            let input_file = sub.value_of("FILE").unwrap();
            let file = get_file(Path::new(input_file));
            let query: Option<logic::Literal> = sub.value_of("QUERY").map(|l| l.parse().unwrap());

            match (file.source().parse::<Modusfile>(), query) {
                (Ok(modus_f), None) => println!(
                    "Parsed {} successfully. Found {} clauses.",
                    input_file,
                    modus_f.0.len()
                ),
                (Ok(modus_f), Some(l)) => match prove_goal(&modus_f, &vec![l.clone()]) {
                    Ok(proofs) => {
                        println!(
                            "{} proof(s) found for query {}",
                            proofs.len(),
                            l.to_string().blue()
                        );
                        // TODO: pretty print proof, we could use the 'colored' library for terminal colors
                    }
                    Err(e) => term::emit(&mut writer.lock(), &config, &file, &e)
                        .expect("Error when printing to stderr."),
                },
                (Err(error), _) => {
                    println!(
                        "❌ Did not parse {} successfully. Error trace:\n{}",
                        input_file.purple(),
                        error
                    )
                }
            }
        }
        _ => (),
    }
}
