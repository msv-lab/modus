use clap::{
    crate_name,
    crate_version,
    crate_authors,
    App,
    Arg
};


fn main() {
    let matches = App::new("Modus")
        .version(crate_version!())
        .author(crate_authors!())
        .arg(Arg::with_name("file")
            .short("f")
            .long("file")
            .value_name("FILE")
            .help("Sets Modusfile")
            .takes_value(true))
        .arg(Arg::with_name("query")
            .short("q")
            .long("query")
            .value_name("TARGET")
            .help("Specifies target image as Datalog fact")
            .takes_value(true))
        .arg(Arg::with_name("proof")
            .short("p")
            .long("proof")
            .help("Prints target proof tree"))
        .arg(Arg::with_name("output")
            .short("o")
            .value_name("FILE")
            .long("output")
            .help("Outputs Dockerfile for given query")
            .takes_value(true))
        .get_matches();

    if let Some(q) = matches.value_of("query") {
        println!("Value for query: {}", q);
    }

}
