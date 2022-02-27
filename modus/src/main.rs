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

mod buildkit;
mod reporting;

use clap::{crate_version, Command, Arg, arg};
use codespan_reporting::{
    files::SimpleFile,
    term::{
        self,
        termcolor::{Color, ColorSpec, StandardStream, WriteColor},
    },
};
use colored::Colorize;
use modus_lib::{analysis::ModusSemantics, sld::tree_from_modusfile};
use modus_lib::transpiler::render_tree;
use modus_lib::*;
use ptree::write_tree;
use std::{ffi::OsStr, fs, path::Path};
use std::{io::Write, path::PathBuf};

use modus_lib::modusfile::Modusfile;

use crate::buildkit::{BuildOptions, DockerBuildOptions};

fn get_file_or_exit(path: &Path) -> SimpleFile<&str, String> {
    let file_name: &str = path
        .file_name()
        .map(|os_str| os_str.to_str())
        .unwrap()
        .unwrap();
    let file_content: String = match fs::read_to_string(path) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading {}: {}", path.display(), err);
            std::process::exit(1);
        }
    };

    SimpleFile::new(file_name, file_content)
}

fn main() {
    let matches = Command::new("modus")
        .version(crate_version!())
        .about("A language for building container images")
        .subcommand_required(true)
        .arg_required_else_help(true)
        .subcommand(
            Command::new("transpile")
                .hide(true)
                .arg(
                    Arg::new("FILE")
                        .required(true)
                        .help("Set the input Modusfile")
                        .index(1),
                )
                .arg(
                    Arg::new("QUERY")
                        .required(true)
                        .help("Specify the build target(s)")
                        .index(2),
                )
        )
        .subcommand(
            Command::new("build")
                .about("Build images.")
                .arg(
                    Arg::new("FILE")
                        .required(false)
                        .long_help("Specify the input Modusfile\n\
                                    The default is to look for a Modusfile in the context directory.")
                        .help("Specify the input Modusfile")
                        .value_name("FILE")
                        .short('f')
                        .long("modusfile")
                        .allow_invalid_utf8(true)
                )
                .arg(
                    Arg::new("CONTEXT")
                        .help("Specify the build context directory")
                        .index(1)
                        .required(true)
                        .allow_invalid_utf8(true),
                )
                .arg(
                    Arg::new("QUERY")
                        .required(true)
                        .help("Specify the target query to build")
                        .index(2),
                )
                .arg(
                    Arg::new("JSON_OUTPUT")
                        .value_name("FILE")
                        .required(false)
                        .min_values(0)
                        .max_values(1)
                        .require_equals(true)
                        .long("json")
                        .help("Output build result as JSON")
                        .long_help("Output build result as JSON\n\
                                    If this flag is specified without providing a file name, output is written to stdout.")
                        .allow_invalid_utf8(true)
                )
                .arg(
                    Arg::new("VERBOSE")
                        .short('v')
                        .long("verbose")
                        .help("Tell docker to print all the output"),
                )
                .arg(
                    Arg::new("NO_CACHE")
                        .long("--no-cache")
                        .help("Ignore all existing build cache"),
                )
                .arg(
                    Arg::new("ADDITIONAL_OPTS")
                        .long("docker-flags")
                        .takes_value(true)
                        .multiple_values(true)
                        .required(false)
                        .help("Pass additional options to docker build")
                )
                .arg(
                    Arg::new("RESOLVE_CONCURRENCY")
                        .long("image-resolve-concurrency")
                        .takes_value(true)
                        .required(false)
                        .default_value("3")
                        .value_name("NUM")
                )
                .arg(
                    Arg::new("EXPORT_CONCURRENCY")
                        .long("image-export-concurrency")
                        .takes_value(true)
                        .required(false)
                        .default_value("8")
                        .value_name("NUM")
                )
                .arg(
                    Arg::new("CUSTOM_FRONTEND")
                        .long("custom-buildkit-frontend")
                        .value_name("IMAGE_REF")
                        .takes_value(true)
                        .required(false)
                        .help("Specify a custom buildkit buildkit frontend to use")
                        .long_help(concat!("Specify a custom frontend to use for buildkit. It must parse a JSON Modus build plan, and invoke relevant buildkit calls.\n\
                                    The default is to use a pre-built one hosted on ghcr.io, with commit id ", env!("GIT_SHA"), ".\n\
                                    This flag allows you to use something other than the default, for example for development on Modus itself."))
                        .default_value(buildkit::FRONTEND_IMAGE),
                )
        )
        .subcommand(
            Command::new("proof")
                .about("Print proof tree of a given query.")
                .arg(
                    Arg::new("FILE")
                        .required(false)
                        .long_help("Set the input Modusfile\n\
                                    The default is to look for a Modusfile in the context directory.")
                        .help("Set the input Modusfile")
                        .value_name("FILE")
                        .short('f')
                        .long("modusfile")
                        .allow_invalid_utf8(true),
                )
                .arg(
                    Arg::new("CONTEXT")
                        .long_help("Specify the directory that contains the Modusfile.\n\
                                    This is for compatibility with the `build` subcommand.")
                        .help("Specify the directory that contains the Modusfile.")
                        .index(1)
                        .required(true)
                        .allow_invalid_utf8(true),
                )
                .arg(
                    Arg::new("QUERY")
                        .required(true)
                        .help("Specify the target to prove")
                        .index(2),
                )
                .arg(arg!(-e --explain "Prints out an explanation of the steps taken in resolution."))
                .arg(arg!(-g --graph "Outputs a (DOT) graph that of the SLD tree traversed in resolution.")),
        )
        .subcommand(
            Command::new("check")
                .about("Analyse a Modusfile and checks the predicate kinds.")
                .arg(
                    Arg::new("FILE")
                        .required(false)
                        .long_help("Set the input Modusfile\n\
                                    The default is to look for a Modusfile in the context directory.")
                        .help("Set the input Modusfile")
                        .value_name("FILE")
                        .short('f')
                        .long("modusfile")
                        .allow_invalid_utf8(true),
                )
                .arg(
                    Arg::new("CONTEXT")
                        .long_help("Specify the directory that contains the Modusfile.\n\
                                    This is for compatibility with the `build` subcommand.")
                        .help("Specify the directory that contains the Modusfile.")
                        .index(1)
                        .required(true)
                        .allow_invalid_utf8(true),
                )
                .arg(arg!(-v --verbose "display the evaluated kinds for all the clauses"))
        )
        .get_matches();

    let out_writer = StandardStream::stdout(codespan_reporting::term::termcolor::ColorChoice::Auto);
    let err_writer = StandardStream::stderr(codespan_reporting::term::termcolor::ColorChoice::Auto);
    let config = codespan_reporting::term::Config::default();

    match matches.subcommand().unwrap() {
        ("transpile", sub) => {
            let input_file = sub.value_of("FILE").unwrap();
            let file = get_file_or_exit(Path::new(input_file));
            let query: modusfile::Expression =
                sub.value_of("QUERY").map(|s| s.parse().unwrap()).unwrap();
            let query = query.without_position();

            let mf: Modusfile = match file.source().parse() {
                Ok(mf) => mf,
                Err(e) => {
                    eprintln!(
                        "❌ Did not parse Modusfile successfully. Error trace:\n{}",
                        e
                    );
                    std::process::exit(1);
                }
            };
            let kind_res = mf.kinds();
            if !analysis::check_and_output_analysis(
                &kind_res,
                &mf,
                false,
                &mut err_writer.lock(),
                &config,
                &file,
            ) {
                std::process::exit(1)
            }

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
        ("build", sub) => {
            let context_dir = sub.value_of_os("CONTEXT").unwrap();
            let input_file = sub
                .value_of_os("FILE")
                .map(PathBuf::from)
                .unwrap_or_else(|| Path::new(context_dir).join("Modusfile"));
            let file = get_file_or_exit(input_file.as_path());
            let query: modusfile::Expression =
                sub.value_of("QUERY").map(|s| s.parse().unwrap()).unwrap();
            let query = query.without_position();

            let mf: Modusfile = match file.source().parse() {
                Ok(mf) => mf,
                Err(e) => {
                    eprintln!(
                        "❌ Did not parse Modusfile successfully. Error trace:\n{}",
                        e
                    );
                    std::process::exit(1);
                }
            };
            let kind_res = mf.kinds();
            if !analysis::check_and_output_analysis(
                &kind_res,
                &mf,
                false,
                &mut err_writer.lock(),
                &config,
                &file,
            ) {
                std::process::exit(1)
            }

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
                    write!(w, "{}", e_str)?;
                    w.set_color(&ColorSpec::new())?;
                    write!(w, "\n")?;
                    w.flush()?;
                    Ok(())
                })()
                .expect("Unable to write to stderr.");
                std::process::exit(1)
            }
            let options = BuildOptions {
                frontend_image: sub.value_of("CUSTOM_FRONTEND").unwrap().to_owned(),
                resolve_concurrency: sub
                    .value_of("RESOLVE_CONCURRENCY")
                    .unwrap()
                    .parse()
                    .unwrap_or_else(|_| {
                        print_build_error_and_exit(
                            "invalid resolve concurrency - expected number",
                            &err_writer,
                        )
                    }),
                export_concurrency: sub
                    .value_of("EXPORT_CONCURRENCY")
                    .unwrap()
                    .parse()
                    .unwrap_or_else(|_| {
                        print_build_error_and_exit(
                            "invalid export concurrency - expected number",
                            &err_writer,
                        )
                    }),
                docker_build_options: DockerBuildOptions {
                    verbose: sub.is_present("VERBOSE"),
                    no_cache: sub.is_present("NO_CACHE"),
                    quiet: false,
                    additional_args: sub
                        .values_of("ADDITIONAL_OPTS")
                        .map(|x| x.map(ToOwned::to_owned).collect())
                        .unwrap_or_default(),
                },
            };
            match buildkit::build(build_plan.clone(), context_dir, &options) {
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
        ("proof", sub) => {
            let should_output_graph = sub.is_present("graph");
            let should_explain = sub.is_present("explain");

            let context_dir = sub.value_of_os("CONTEXT").unwrap();
            let input_file = sub
                .value_of_os("FILE")
                .map(PathBuf::from)
                .unwrap_or_else(|| Path::new(context_dir).join("Modusfile"));
            let file = get_file_or_exit(input_file.as_path());
            let query: Option<modusfile::Expression> =
                sub.value_of("QUERY").map(|s| s.parse().unwrap());
            let query = query.map(|q| q.without_position());

            match (file.source().parse::<Modusfile>(), query) {
                (Ok(modus_f), None) => println!(
                    "Parsed successfully. Found {} clauses.",
                    modus_f.0.len()
                ),
                (Ok(modus_f), Some(e)) => {
                    let kind_res = modus_f.kinds();
                    if !analysis::check_and_output_analysis(
                        &kind_res,
                        &modus_f,
                        false,
                        &mut err_writer.lock(),
                        &config,
                        &file,
                    ) {
                        std::process::exit(1)
                    }

                    let max_depth = 175;
                    let (goal, clauses, sld_result) =
                        tree_from_modusfile(modus_f, e.clone(), max_depth, true);

                    if should_output_graph {
                        render_tree(&clauses, sld_result, &mut out_writer.lock());
                    } else if should_explain {
                        let tree_item = sld_result.tree.explain(&clauses);
                        write_tree(&tree_item, &mut out_writer.lock())
                            .expect("Error when printing tree to stdout.");
                    } else {
                        let proof_result =
                            Result::from(sld_result).map(|t| sld::proofs(&t, &clauses, &goal));
                        match proof_result {
                            Ok(proofs) => {
                                println!(
                                    "{} proof(s) found for query {}",
                                    proofs.len(),
                                    e.to_string().underline()
                                );

                                for (_, proof) in proofs {
                                    proof.pretty_print(&clauses, &kind_res.pred_kind).expect("error when printing");
                                }
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
                    eprintln!(
                        "❌ Did not parse Modusfile successfully. Error trace:\n{}",
                        error
                    );
                    std::process::exit(1);
                }
            }
        }
        ("check", sub) => {
            let context_dir = sub.value_of_os("CONTEXT").unwrap();
            let input_file = sub
                .value_of_os("FILE")
                .map(PathBuf::from)
                .unwrap_or_else(|| Path::new(context_dir).join("Modusfile"));
            let file = get_file_or_exit(input_file.as_path());

            let is_verbose = sub.is_present("verbose");

            match file.source().parse::<Modusfile>() {
                Ok(mf) => {
                    let kind_res = mf.kinds();
                    if !analysis::check_and_output_analysis(
                        &kind_res,
                        &mf,
                        is_verbose,
                        &mut err_writer.lock(),
                        &config,
                        &file,
                    ) {
                        std::process::exit(1)
                    }
                }
                Err(e) => {
                    eprintln!(
                        "❌ Did not parse Modusfile successfully. Error trace:\n{}",
                        e
                    );
                    std::process::exit(1);
                }
            }
        }
        _ => (),
    }
}
