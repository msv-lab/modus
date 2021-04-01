# Modus

Modus is a container build system that allows to define parametrized and recursive [multi-stage Docker builds](https://docs.docker.com/develop/develop-images/multistage-build/). Build rules are defined in a Modusfile, a variant of Dockerfile where `FROM ...` instructions are replaced with more general `RULE ...` instructions that express build stages and their dependencies as [Datalog](https://en.wikipedia.org/wiki/Datalog) rules. Given a Modusfile (the intentional database) and a target image (a Datalog fact), Modus generates a multi-stage Dockerfile for constructing the target image. The generated multi-stage build corresponds to the shortest proof of the fact (the target image) from the existential database (base images).

The advantages of Modus are the following:

1. Modus makes build definitions more flexible and reduces code duplication.
2. Modusfiles are intiutive and minimally differ from Dockerfiles.
3. The build system has a well-defined semantics.

Modus is written in Rust, and can be compiled with `cargo build --release`.

## Motivating example

Assume we want to use Docker to compare the outputs of the same program compiled with and without optimizations. The Modusfile below accomplishes it by defining three stages. The stage `build`, which is parametrized with the compiler flags `CFLAGS` and which depends on the image `gcc:4.9`, compiles the program `foo.c`. The stage `test`, which depends on the previous stage `build`, executes a test. Finally, the final stage `compare` depends on two instances of `test` with the flags `-O0` and `-O3` respectively. `compare` first copies the output file from the second instance of `test` (aliased as `test_opt` because `test` is ambiguous), and then compares it with the output file of the first instance of `test`. 

    RULE build(CFLAGS) :- image_tag(gcc, 4.9)
    RUN gcc ${CFLAGS} -c foo.c -o foo

    RULE test(CFLAGS) :- build(CFLAGS)
    RUN ./foo 1 2 3 > /output.txt

    RULE compare() :- test("-O0"), test("-O3") as test_opt
    COPY --from=test_opt /output.txt /output_opt.txt
    RUN diff /output.txt /output_opt.txt

    QUERY compare()

This build can be executed by running 

    cat Modusfile | modus | docker build -

The default query can be specified in Modusfile using 'QUERY compare()' instruction.

Modus can also output a proof tree that shows how the target image is constructed from base images:

    $ cat Modusfile | modus --proof
    compare()
    ├── test("-O0")
    │   └── build("-O0")
    │       └── image_tag(gcc, 4.9)
    └── test("-O3") as test_opt
        └── build("-O3")
            └── image_tag(gcc, 4.9)
   
## Documentation

- Examples
  - [Translating Dockerfile to Modusfile](doc/example-nullary-stages.md)
  - [Modusfile with parametrised stages](doc/example-parametrized-stages.md)
  - [Modusfile with recursive stages](doc/example-recursive-stages.md)
- Manual
  - [Modusfile syntax](doc/manual-modusfile-syntax.md)
  - [Modusfile semantics](doc/manual-modusfile-semantics.md)
  - [Command-line tool](doc/manual-command-line-tool.md)