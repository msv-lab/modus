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
mod reporting;
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
    files::SimpleFile,
    term::{
        self,
        termcolor::{Color, ColorSpec, StandardStream, WriteColor},
    },
};
use colored::Colorize;
use std::{ffi::OsStr, fs, path::Path};
use std::{io::Write, path::PathBuf};
use transpiler::render_tree;

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
                        .required(false)
                        .long_help("Specifies the input Modusfile\n\
                                    The default is to look for a Modusfile in the context directory.")
                        .help("Specify the input Modusfile")
                        .value_name("FILE")
                        .short("f")
                        .long("modusfile")
                )
                .arg(
                    Arg::with_name("CONTEXT")
                        .help("Specify the build context directory")
                        .index(1)
                        .required(true),
                )
                .arg(
                    Arg::with_name("QUERY")
                        .required(true)
                        .help("Specify the target query to build")
                        .index(2),
                )
                .arg(
                    Arg::with_name("JSON_OUTPUT")
                        .value_name("FILE")
                        .required(false)
                        .min_values(0)
                        .max_values(1)
                        .require_equals(true)
                        .long("json")
                        .help("Output build result as JSON")
                        .long_help("Output build result as JSON\n\
                                    If this flag is specified without providing a file name, output is written to stdout.")
                )
                .arg(
                    Arg::with_name("VERBOSE")
                        .short("v")
                        .long("verbose")
                        .help("Tell docker to print all the output"),
                )
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
                )
                .arg_from_usage("-g --graph 'Outputs a (DOT) graph that of the SLD tree traversed in resolution.'"),
        )
        .get_matches();

    let out_writer = StandardStream::stdout(codespan_reporting::term::termcolor::ColorChoice::Auto);
    let err_writer = StandardStream::stderr(codespan_reporting::term::termcolor::ColorChoice::Auto);
    let config = codespan_reporting::term::Config::default();

    match matches.subcommand() {
        ("transpile", Some(sub)) => {
            let input_file = sub.value_of("FILE").unwrap();
            let file = get_file(Path::new(input_file));
            let query: logic::Literal = sub.value_of("QUERY").map(|s| s.parse().unwrap()).unwrap();
            let query = query.with_position(None);

            let mf: Modusfile = file.source().parse().unwrap();

            let df_res = transpiler::transpile(mf, query);

            match df_res {
                Ok(df) => println!("{}", df),
                Err(e) => {
                    for diag_error in e {
                        term::emit(&mut err_writer.lock(), &config, &file, &diag_error)
                            .expect("Error when printing to stderr.")
                    }
                    std::process::exit(1)
                }
            }
        }
        ("build", Some(sub)) => {
            let context_dir = sub.value_of_os("CONTEXT").unwrap();
            let input_file = sub
                .value_of_os("FILE")
                .map(|x| PathBuf::from(x))
                .unwrap_or_else(|| Path::new(context_dir).join("Modusfile"));
            let file = get_file(input_file.as_path());
            let query: logic::Literal = sub.value_of("QUERY").map(|s| s.parse().unwrap()).unwrap();
            let query = query.with_position(None);

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
                    for diag_error in e {
                        term::emit(&mut err_writer.lock(), &config, &file, &diag_error)
                            .expect("Error when printing to stderr.")
                    }
                    std::process::exit(1)
                }
            };
            fn print_build_error_and_exit(e_str: &str, w: &StandardStream) -> ! {
                let mut w = w.lock();
                (move || -> std::io::Result<()> {
                    w.set_color(ColorSpec::new().set_fg(Some(Color::Red)).set_bold(true))?;
                    write!(w, "build error")?;
                    w.set_color(&ColorSpec::new())?;
                    write!(w, ": ")?;
                    w.set_color(ColorSpec::new().set_bold(true))?;
                    write!(w, "{}\n", e_str)?;
                    w.flush()?;
                    Ok(())
                })()
                .expect("Unable to write to stderr.");
                std::process::exit(1)
            }
            let verbose = sub.is_present("VERBOSE");
            match buildkit::build(&build_plan, context_dir, verbose) {
                Err(e) => {
                    print_build_error_and_exit(&e.to_string(), &err_writer);
                }
                Ok(image_ids) => {
                    if sub.is_present("JSON_OUTPUT") {
                        let json_out_name;
                        let mut json_out_f;
                        let mut json_out_stdout;
                        let json_out: &mut dyn Write;
                        if let Some(o_path) = sub.value_of_os("JSON_OUTPUT") {
                            json_out = match std::fs::File::create(o_path) {
                                Ok(f) => {
                                    json_out_f = f;
                                    &mut json_out_f
                                }
                                Err(e) => {
                                    print_build_error_and_exit(
                                        &format!(
                                            "Unable to open {} for writing: {}.",
                                            o_path.to_string_lossy(),
                                            &e
                                        ),
                                        &err_writer,
                                    );
                                }
                            };
                            json_out_name = o_path;
                        } else {
                            json_out_stdout = std::io::stdout();
                            json_out = &mut json_out_stdout;
                            json_out_name = OsStr::new("stdout");
                        }
                        if let Err(e) = reporting::write_build_result(
                            json_out,
                            &json_out_name.to_string_lossy(),
                            &build_plan,
                            &image_ids[..],
                        ) {
                            print_build_error_and_exit(&e, &err_writer);
                        }
                    }
                }
            }
        }
        ("proof", Some(sub)) => {
            let should_output_graph = sub.is_present("graph");
            let input_file = sub.value_of("FILE").unwrap();
            let file = get_file(Path::new(input_file));
            let query: Option<logic::Literal> = sub.value_of("QUERY").map(|l| l.parse().unwrap());
            let query = query.map(|lit| lit.with_position(None));

            match (file.source().parse::<Modusfile>(), query) {
                (Ok(modus_f), None) => println!(
                    "Parsed {} successfully. Found {} clauses.",
                    input_file,
                    modus_f.0.len()
                ),
                (Ok(modus_f), Some(l)) => {
                    if should_output_graph {
                        render_tree(&modus_f, &vec![l.clone()], &mut out_writer.lock());
                    } else {
                        match prove_goal(&modus_f, &vec![l.clone()]) {
                            Ok(proofs) => {
                                println!(
                                    "{} proof(s) found for query {}",
                                    proofs.len(),
                                    l.to_string().blue()
                                );
                                // TODO: pretty print proof, we could use the 'colored' library for terminal colors
                            }
                            Err(e) => {
                                for diag_error in e {
                                    term::emit(&mut err_writer.lock(), &config, &file, &diag_error)
                                        .expect("Error when printing to stderr.")
                                }
                            }
                        }
                    }
                }
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
