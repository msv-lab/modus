# Modus

Modus is a Datalog-based language for building Docker images. Modus extends [Dockerfile](https://docs.docker.com/engine/reference/builder/)'s syntax with [Horn clauses](https://en.wikipedia.org/wiki/Horn_clause) for expressing build dependencies. Modus has the following advantages:

- Parameterised [build stages](https://docs.docker.com/develop/develop-images/multistage-build/), whose arguments are resolved automatically using a Datalog solver, improve __modularity__, enable __complex workflows__, and address longstanding [usability](https://github.com/moby/moby/issues/32100) [issues](https://github.com/moby/moby/issues/32100).

- Modus allows automatically __resolving dependencies__ such as versions, base images, compilations flags, etc. Modus provides first-class support for [SemVer](https://semver.org/) constraints.

- Modus computes build trees that optimise the __build time__; combined with [BuildKit](https://github.com/moby/buildkit), Modus enables efficient __parallel builds__ of groups of related images; Modus provides fine-grained control for optimising __image size__.

- Modus is a backwards-compatible extension, a Dockerfile is also a valid Modus input format. Modus is __easy__ to use and understand because it is declarative and non-Turing-complete.

Modus uses semantic versioning; until version 1.0 is declared, breaking changes are possible. We welcome bug reports and feature requests submitted through [GitHub Issues](https://github.com/mechtaev/modus/issues).

## Overview

Docker container images are built using sequences of instructions specified in Dockerfiles. Dockerfiles allow grouping sequences of instructions into units called [build stages](https://docs.docker.com/develop/develop-images/multistage-build/). Build stages enable code reuse, optimisation of build times and image sizes, etc.

Compared to functions/procedures in general-purpose programming languages, build stages do not provide [procedural abstraction](http://www.eecs.qmul.ac.uk/~mmh/AMCM048/abstraction/procedural.html). Consider this example of a typical Dockerfile with build stages:

```Dockerfile
FROM a AS b
ARG X
RUN ...   # use ${X}

FROM c AS d
ARG Y
RUN ...   # use ${Y}

FROM b AS e
ARG Z
COPY --from=d ...
RUN ...   # use ${Z}
```

In this example, the stage `e`, parametrised with the variable `Z`, depends on the stages `b` and `d`, which are parametrised with the variables `X` and `Y` respectively. When the build is executed, the variables `X`, `Y` and `Z` have to be set through command line options, e.g. `--build-arg X=1 --build-arg Y=2 --build-arg Z=1`. This approach has several problems:
- There can be implicit dependencies between `e` and the arguments of `b` and `d` that are not reflected in the code. For example, `e` may require `d` with `Y=2`.
- The user has to find and correctly set all relevant arguments in all transitively dependent stages to build a given stage.
- The same stage cannot be reused with different parameter values within the same build.

Modus addresses these issues by replacing the instruction `FROM` with the instruction `RULE` that explicitly describes dependencies between build stages and their parameters:

```Dockerfile
RULE b(X) :- a
RUN ...   # use ${X}

RULE d(Y) :- c
RUN ...   # use ${Y}

RULE e(X, Z) :- b(X), d(2)
COPY --from=d ...
RUN ...   # use ${Z}
```

Effectively, build dependency resolution is reduced to solving a set of [Horn clauses](https://en.wikipedia.org/wiki/Horn_clause) such as `e(X, Y) :- b(X), d(2)`. Specifically, build definitions in Modus can be thought of as a deductive database, and build targets are specified as queries to this database. Then, the build tree corresponds to the minimal proof of a given query from _facts_ representing existing images.

Apart from improving modularity and clarity of build definitions, Horn clauses enables additional features such as automatic resolution of software versions and compilation flags, building groups of related images, and more.

## Motivating example

Assume that we would like to containerise the application `app`. Suppose also that `app` depends on the library `library`, different versions of which depend on different versions of Python. We would like to have two build mode: "development" mode for development and testing, and "production" mode with a smaller image and better security. The Modusfile below defines a parametrised build that (1) automatically resolves dependencies, and (2) supports both "development" and "production" modes without code duplication. 

The example below uses special literals for Docker images, e.g. `i"python:3.8"`, and for SemVer versions, e.g. `v"1.3.0-alpha"`:

```Dockerfile
# Relation between the library version and the required Python version:
RULE library_python(LIB_VER, v"3.4") :- LIB_VER < v"1.1.0"
RULE library_python(LIB_VER, v"3.5") :- \
       LIB_VER >= v"1.1.0", LIB_VER < v"1.3.0-alpha"
RULE library_python(LIB_VER, v"3.8") :- LIB_VER >= v"1.3.0-alpha"

# Relation between build modes (development/production)
# and library targets (debug/release):
RULE mode_target(development, "debug")
RULE mode_target(production, "release")

# The build stage that downloads and compiles the library.
# Python's version and the make target are resolved based
# on the library version and the build mode:
RULE library(LIB_VER, MODE) :- \
       image(i"python:${PYTHON_VER}"), \
       library_python(LIB_VER, PYTHON_VER), \
       mode_target(MODE, TARGET)
RUN apt-get install make
RUN wget https://library.com/releases/library-v${LIB_VER}.tar.gz && \
    tar xf library-v${LIB_VER}.tar.gz && \
    mv library-v${VERSION}/ /my_lib
WORKDIR /my_lib
RUN make ${TARGET}

# In development mode, use the "library" build stage as the parent image,
# and additionally install development tools (Pylint):
RULE dependencies(LIB_VER, development) :- library(LIB_VER)
RUN pip install pylint

# In production mode, use Alpine as the parent image,
# and copy compiled binaries from the "library" build stage:
RULE dependencies(LIB_VER, production) :- \
       image(i"python:${PYTHON_VER}-alpine"), \
       library(LIB_VER), \
       library_python(LIB_VER, PYTHON_VER)
COPY --from=library /my_lib /my_lib

# Copy app's source code to the appropriate parent image:
RULE app(LIB_VER, MODE) :- dependencies(LIB_VER, MODE)
COPY . /my_app
```

For a given query, Modus generates a Dockerfile that builds the corresponding targets, using the `modus-transpile` tool. In Bash, the above build can be executed by running 

    docker build . -f <(modus-transpile Modusfile --query 'app(v"1.2.5", production)')

Modus can print the proof tree of a given query that shows how the target image is constructed from parent images:

    $ modus-transpile Modusfile --query 'app(v"1.2.5", production)' --proof
    app(v"1.2.5", production)
    └── dependencies(v"1.2.5", production)
        ├── image(i"python:3.5-alpine")
        ├── library(v"1.2.5", production)
        │   ├── image(i"python:3.5")
        │   ├╶╶ library_python(v"1.2.5", v"3.5")
        │   └╶╶ mode_target(production, "debug")
        └╶╶ library_python(v"1.2.5", v"3.5")

Predicates that do not represent images (_imageless_ predicates) in the proof tree are preceded with `╶╶`.

## Documentation

- [Installation](doc/installation.md)
- [Tutorial](doc/tutorial.md)
- [Language reference](doc/language-reference.md)
- [Command-line tool](doc/command-line-tool.md)
- Examples
  - [Optimising image size](doc/example/optimising-image-size.md)
