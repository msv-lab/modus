# Modus

Modus is a Datalog-based language for building Docker images. Modus extends [Dockerfile](https://docs.docker.com/engine/reference/builder/)'s syntax with [Horn clauses](https://en.wikipedia.org/wiki/Horn_clause) for representing build dependencies. Modus has the following advantages:

- __Modularity:__ Modus allows splitting build definitions into independent reusable parts called _parameterised build stages_. Parametrised build stages are [build stages](https://docs.docker.com/develop/develop-images/multistage-build/) with arguments whose values are resolved automatically using a Datalog solver. This makes build stages feel closer to functions, and addresses longstanding usability issues (e.g. [#32100](https://github.com/moby/moby/issues/32100), [#37345](https://github.com/moby/moby/issues/32100)).

- __Dependency resolution:__ Modus allows automatically resolving build dependencies such as versions, base images, compilations flags, etc. Modus provides first-class support for [SemVer](https://semver.org/) constraints.

- __Efficiency:__ For a given target, Modus computes a build tree that minimises the number of executed build steps. Combined with [BuildKit](https://github.com/moby/buildkit), Modus enables efficient parallel builds of groups of related images. Parametrised build stages provide fine-grained control of the resulting image size.

- __Ease of use:__ Modus is a backwards-compatible extension: a Dockerfile is also a valid _Modusfile_, Modus input format. Modus extends Dockerfile's syntax with a new instruction `RULE`, which can be used instead of Docker's `FROM` instruction to describe a build stage and its dependencies in the form of a Horn clause. Modus is declarative and non-Turing-complete.

We welcome bug reports and feature requests submitted through [GitHub Issues](https://github.com/mechtaev/modus/issues).

## Overview

Docker container images are built using sequences of instructions specified in Dockerfiles. Dockerfiles allow grouping sequences of instructions into units called [build stages](https://docs.docker.com/develop/develop-images/multistage-build/). Build stages are important for code reuse, maintenance, optimisation of the build time and the image size, etc.

Compared to functions/procedures in general-purpose programming languages, build stages do not provide [procedural abstraction](http://www.eecs.qmul.ac.uk/~mmh/AMCM048/abstraction/procedural.html). Consider this example of a typical Dockerfile with build stages:

```Dockerfile
FROM alpine AS a
ARG X
RUN ...   # use ${X}

FROM ubuntu AS b
ARG Y
RUN ...   # use ${Y}

FROM a AS c
COPY --from=b ...
RUN ...
```

In this example, the stage `c` depends on the stages `a` and `b`, which are parametrised with the variables `X` and `Y` respectively. When the build is executed, the variables `X` and `Y` have to be set through the command line options `--build-arg A=1 --build-arg B=2`. This approach has several problems:
- There could be implicit dependencies between `c` and the arguments of `a` and `b` that are not reflected in the code. For example, `c` may require `b` with `Y=2`.
- The user has to find and correctly set all relevant arguments in all transitively dependent stages to build a given stage.
- The same stage cannot be reused with different values of the parameters within the same build.

Modus addresses these issues by replacing the instruction `FROM` with the instruction `RULE` that explicitly describes dependencies between build stages and their parameters:

```Dockerfile
RULE a(X) :- image(i"alpine")
RUN ...   # use ${X}

RULE b(Y) :- image(i"ubuntu")
RUN ...   # use ${Y}

RULE c(X) :- a(X), b(2)
COPY --from=b ...
RUN ...
```

where `i"alpine"` and `i"ubuntu"` are special literals for image objects.

Note that `c(X) :- a(X), b(2)` is a Horn clause. The use of Horn clauses enables Modus to take advantage of the power of logic programming languages such as Datalog/Prolog. For example, Modus can automatic resolve SemVer versions, and build multiple images at the same time.


## Motivating example

Assume that we would like to containerise the application `app`. Suppose also that `app` depends on the library `lib`, different versions of which depend on different versions of Python. We would like to have two build mode: "dev" mode for development and testing, and "production" mode with a smaller image and better security. The Modusfile below defines a parametrised build that (1) automatically resolves dependencies, and (2) supports both "dev" and "production" modes without code duplication. 

The example below uses special literals for Docker images, e.g. `i"python:3.8"`, and for SemVer versions, e.g. `v"1.3.0-alpha"`:

```Dockerfile
# Python versions required by different versions of the library:
RULE lib_python(VERSION, v"3.4") :- VERSION < v"1.1.0"
RULE lib_python(VERSION, v"3.5") :- \
       VERSION >= v"1.1.0", VERSION < v"1.3.0-alpha"
RULE lib_python(VERSION, v"3.8") :- VERSION >= v"1.3.0-alpha"

# Library build targets (debug/release) for different build modes (dev/production):
RULE mode_target("dev", "debug")
RULE mode_target("production", "release")

# 'lib' build stage that downloads and compiles the library. 
# Python's version and the build target are resolved
# based on the library version and the build mode:
RULE lib(VERSION, MODE) :- \
       image(i"python:${PYTHON}"), \
       lib_python(VERSION, PYTHON), \
       mode_target(MODE, TARGET)
RUN apt-get install make
RUN wget https://example.com/releases/example-v${VERSION}.tar.gz && \
    tar xf example-v${VERSION}.tar.gz && \
    mv example-v${VERSION}/ /my_lib
WORKDIR /my_lib
RUN make ${TARGET}

# For the dev mode, use the "lib" build stage as the parent
# and additionally install development tools (Pylint):
RULE install_deps(VERSION, "dev") :- lib(VERSION)
RUN pip install pylint

# For the production mode, use Alpine as the parent image, 
# and copy compiled binaries from the "lib" build stage:
RULE install_deps(VERSION, "production") :- \
       image(i"python:${PYTHON}-alpine"), \
       lib(VERSION), \
       lib_python(VERSION, PYTHON)
COPY --from=lib /my_lib /my_lib

# Copy app's source code to the appropriate parent image:
RULE app(VERSION, MODE) :- install_deps(VERSION, MODE)
COPY . /my_app
```

Modus provides a source-to-source translator from Modusfiles to Dockerfiles, `modus-transpile`. In Bash shell, the above build can be executed by running 

    docker build . -f <(modus-transpile Modusfile --query 'app(v"1.2.5", "production")')

where the `--query` option specifies the target image.

Modus can print the proof tree of a given query that shows how the target image is constructed from parent images:

    $ modus-transpile Modusfile --query 'app(v"1.2.5", "release")' --proof
    app(v"1.2.5", "production")
    └── base(v"1.2.5", "production")
        ├── image(i"python:3.5-alpine")
        ├── lib(v"1.2.5", "production")
        │   ├── image(i"python:3.5")
        │   ├╶╶ lib_python(v"1.2.5", v"3.5")
        │   └╶╶ mode_target("production", "debug")
        └╶╶ lib_python(v"1.2.5", v"3.5")

Predicates that do not represent images (_imageless_ predicates) in the proof tree are preceded with `╶╶`.

## Documentation

- [Installation](doc/installation.md)
- [Tutorial](doc/tutorial.md)
- [Language reference](doc/language-reference.md)
- [Command-line tool](doc/command-line-tool.md)
- Examples
  - [Optimising image size](doc/example/optimising-image-size.md)
