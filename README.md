# Modus

Modus is a Datalog-based container build system that extends the capabilities of [multi-stage Docker builds](https://docs.docker.com/develop/develop-images/multistage-build/). Modus allows to express parametrised build stages, and resolves dependencies using a Datalog solver. This has several advantages over existing approaches: (1) reduced code duplication, (2) modular build definitions, and (3) automatic, optimal dependency resolution.

A Modus build is defined in a Modusfile, a variant of Dockerfile where `FROM ...` instructions are replaced with more general `RULE ...` instructions that express build stages as [Datalog](https://en.wikipedia.org/wiki/Datalog) rules. Modusfiles are intuitive and do not require the knowledge of Datalog. However, if you are familiar with Datalog, then the semantics of the build system can be explained as follows. Datalog facts correspond to container images, Datalog rules are associated with build stages, Datalog constants are strings, and Datalog variables correspond to Dockerfile's `ARG` variables. For a given target image, Modus computes the required build tasks as the minimal proof tree of the target image from the parent images. The minimality of the proof tree is defined w.r.t. the number of unique intermediate images.

If you have used Modus in your project, please [share your experience](https://docs.google.com/forms/d/e/1FAIpQLSctraHPE-vx9m6Mc6APfCykSGzP-ShE93BO-R57helgw82_4A/viewform?usp=sf_link) with us. We also welcome bug reports and feature requests submitted through [Issues](https://github.com/mechtaev/modus/issues).

## Motivating example

The Modusfile below is designed to compare the outputs of the program `foo.c` compiled with and without optimizations. The stage `compile`, which depends on the image `gcc:4.9` and is parametrized with the compiler flags `CFLAGS`, compiles the program `foo.c`. The stage `test`, which depends on the previous stage `compile`, executes a test. Finally, the final stage `compare` depends on two instances of `test` with the flags `-O0` and `-O3` respectively. `compare` first copies the output file from the second instance of `test` (aliased as `test_opt` because of ambiguity), and then compares it with the output of the first instance.

    RULE compile(CFLAGS) :- image("gcc:4.9")
    COPY foo.c .
    RUN gcc ${CFLAGS} -c foo.c -o foo

    RULE test(CFLAGS) :- compile(CFLAGS)
    RUN ./foo 1 2 3 > /output.txt

    RULE compare() :- test("-O0"), test("-O3") as test_opt
    COPY --from=test_opt /output.txt /output_opt.txt
    RUN diff /output.txt /output_opt.txt

Modus provides a source-to-source translator from Modusfiles to Dockerfiles, `modus-transpile`. In Bash shell, the above build can be executed by running 

    docker build . -f <(modus-transpile Modusfile --query 'compare()')

where the `--query` option specifies the target image. The default query can be set in Modusfile by adding `QUERY compare()`.

Modus can print the proof tree of a given query that shows how the target image is constructed from parent images:

    $ modus-transpile Modusfile --query 'compare()' --proof
    compare()
    ├── test("-O0")
    │   └── compile("-O0")
    │       └── image("gcc:4.9")
    └── test("-O3") as test_opt
        └── compile("-O3")
            └── image("gcc:4.9")

In contrast with the Modusfile, in a Dockerfile the stages `compile` and `test` would have to be duplicated for each compiler option.
   
## Documentation

- Examples
  - [Translating Dockerfile to Modusfile](doc/example-nullary-stages.md)
  - [Avoiding code duplication](doc/example-avoiding-code-duplication.md)
  - [Optimal dependency resolution](doc/example-optimal-dependency-resolution.md)
  - [Recursive build stages](doc/example-recursive-stages.md)
- Manual
  - [Installation instructions](doc/manual-installation.md)
  - [Modusfile reference](doc/manual-modusfile-reference.md)
  - [Command-line tool](doc/manual-command-line-tool.md)
- Development
  - [Roadmap](doc/development-roadmap.md)