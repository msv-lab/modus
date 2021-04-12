# Modus

Modus is a container image build system that allows to define parametrised [build stages](https://docs.docker.com/develop/develop-images/multistage-build/) and resolves dependencies using a Datalog solver. It makes build definitions concise, modular and reusable, helps to build smaller images, and optimises build time.

Docker images are typically built using Dockerfiles that contain sequences of commands needed to build a given image. An important limitation of Dockerfiles is their limited support for build parameters. Typically, Dockerfiles hard-code paths, versions and configuration options. This leads to hard-to-maintain and inefficient build configurations.

Modus addresses Dockerfile's limitations by introducing _parametrised build stages_, named sequences of build instructions parametrised with lists of strings. These strings can represent, for example, library versions, compiler flags or filesystem paths. Parametrised build stages can be viewed as functions for building images. Injecting parameters solves only half of the problem - the dependencies between parameters have to be consistently resolved. To resolve dependencies, Modus uses [Datalog](https://en.wikipedia.org/wiki/Datalog), a declarative non-Turing complete language that naturally maps to container build definitions. Modus also extends Datalog with constraints, e.g. over [SemVer](https://semver.org/) versions.

If you have used Modus in your project, please [share your experience](https://docs.google.com/forms/d/e/1FAIpQLSctraHPE-vx9m6Mc6APfCykSGzP-ShE93BO-R57helgw82_4A/viewform?usp=sf_link) with us. We also welcome bug reports and feature requests submitted through [Issues](https://github.com/mechtaev/modus/issues).

## Motivating example

A Modus build is defined in a Modusfile, a variant of Dockerfile where `FROM ...` instructions are replaced with more general `RULE ...` instructions that express build stages as Datalog rules. The Modusfile below is designed to compare the outputs of the program `foo.c` compiled with different versions of GCC. The stage `compile`, parametrized with the gcc version `GCC_VERSION` and the compiler flags `CFLAGS`, compiles the program `foo.c`. The stage `test`, which depends on the previous stage `compile`, executes a test. Finally, the final stage `compare` depends on two instances of `test` that uses GCC versions `V1` and `V2` respectively. `compare` first copies the output file from the second instance of `test` (aliased as `test2` because of ambiguity), and then compares it with the output of the first instance.

    RULE compile(GCC_VERSION, CFLAGS) :- image("gcc:${GCC_VERSION}")
    COPY foo.c .
    RUN gcc ${CFLAGS} -c foo.c -o foo

    RULE test(GCC_VERSION) :- compile(GCC_VERSION, "-O2")
    RUN ./foo 1 2 3 > /output.txt

    RULE compare(V1, V2) :- test(V1), test(V2) as test2
    COPY --from=test2 /output.txt /output2.txt
    CMD diff /output.txt /output2.txt

Modus provides a source-to-source translator from Modusfiles to Dockerfiles, `modus-transpile`. In Bash shell, the above build can be executed by running 

    docker build . -f <(modus-transpile Modusfile --query 'compare("4.8", "4.9")')

where the `--query` option specifies the target image.

Modus can print the proof tree of a given query that shows how the target image is constructed from parent images:

    $ modus-transpile Modusfile --query 'compare("4.8", "4.9")' --proof
    compare("4.8", "4.9")
    ├── test("4.8")
    │   └── compile("4.8", "-O2")
    │       └── image("gcc:4.8")
    └── test("4.9") as test2
        └── compile("4.9", "-O2")
            └── image("gcc:4.9")

In contrast with the Modusfile, in a Dockerfile the stages `compile` and `test` would have to be duplicated for each compiler option.
   
## Documentation

- Examples
  - [Avoiding Code Duplication](doc/example-avoiding-code-duplication.md)
  - [Reusing Build Definitions](doc/example-reusing-build-definitions.md)
  - [Improving Build Speed](doc/example-improving-build-speed.md)
  - [Optimising Image Size](doc/example-optimising-image-size.md)
  - [Managing Versions with SemVer Constaints](doc/example-semver-constraints.md)
- Manual
  - [Installation instructions](doc/manual-installation.md)
  - [Modusfile reference](doc/manual-modusfile-reference.md)
  - [Command-line tool](doc/manual-command-line-tool.md)
- Development
  - [Conceptual Overview](doc/development-conceptual-overview.md)
  - [Roadmap](doc/development-roadmap.md)
