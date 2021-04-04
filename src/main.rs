use clap::{
    crate_version,
    App,
    Arg
};


fn main() {
    let matches = App::new("modus-transpile")
        .version(crate_version!())
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

    if let Some(q) = matches.value_of("query") {
        println!("Value for query: {}", q);
    }

}
