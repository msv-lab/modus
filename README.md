# Modus

Modus is a language for building Docker images that extends [Dockerfile](https://docs.docker.com/engine/reference/builder/)'s syntax with [Datalog](https://en.wikipedia.org/wiki/Datalog) rules. Modus has the following advantages:

- __Modularity:__ Modus allows splitting build definitions into independent reusable parts called _parameterised build stages_. Parametrised build stages are [build stages](https://docs.docker.com/develop/develop-images/multistage-build/) with arguments whose values are resolved automatically using a Datalog solver. This makes build stages feel closer to functions, and addresses longstanding usability issues (e.g. [#32100](https://github.com/moby/moby/issues/32100), [#37345](https://github.com/moby/moby/issues/32100)).

- __Dependency resolution:__ Modus allows automatically resolving build dependencies such as versions, base images, compilations flags, etc. Modus provides first-class support for [SemVer](https://semver.org/) constraints.

- __Efficiency:__ For a given target, Modus computes a build tree that minimises the number of executed build steps. Combined with [BuildKit](https://github.com/moby/buildkit), Modus enables efficient parallel builds of groups of related images. Parametrised build stages provide fine-grained control of the resulting image size.

- __Ease of use:__ A Dockerfile is also a valid _Modusfile_, Modus input format. Modus extends Dockerfile's syntax with a new instruction `RULE`. `RULE` can be used instead of Docker's `FROM` instruction to describe a build stage and its dependencies in the form of a Datalog rule. It can also be used to define logical constraints on build parameters. Datalog is a non-Turing complete language that naturally maps to build definitions, and is easy to reason about and understand. 

We welcome bug reports and feature requests submitted through [GitHub Issues](https://github.com/mechtaev/modus/issues).

## Motivating example

Assume that we would like to containerise the application `app`. `app` depends on the library `lib`, different versions of which depend on different versions of Python. We also would like to have two build mode: "dev" mode for development and testing, and "production" mode with a smaller image and better security. The Modusfile below defines a parametrised build that (1) automatically resolves dependencies, and (2) supports both "dev" and "production" modes without code duplication. 

The example below uses special literals for Docker images, e.g. `i"python:3.8"`, and for SemVer versions, e.g. `v"1.3.0-alpha"`:

```Dockerfile
# Python versions required by different versions of the library:
RULE required_python(VER, v"3.4"): VER < v"1.1.0"
RULE required_python(VER, v"3.5"): VER >= v"1.1.0" & VER < v"1.3.0-alpha"
RULE required_python(VER, v"3.8"): VER >= v"1.3.0-alpha"

# Library build targets (debug/release) for different build modes (dev/production):
RULE build_target("dev", "debug")
RULE build_target("production", "release")

# 'lib' build stage that downloads and compiles the library. 
# Python's version and the build target are resolved automatically
# based on the library version and the build mode:
RULE lib(VERSION, MODE): image(i"python:${PYTHON}") & \
                         required_python(VERSION, PYTHON) & \
                         build_target(MODE, TARGET)
RUN apt-get install make
RUN wget https://example.com/releases/example-v${VERSION}.tar.gz && \
    tar xf example-v${VERSION}.tar.gz && \
    mv example-v${VERSION}/ /my_lib
WORKDIR /my_lib
RUN make ${TARGET}

# For the dev mode, use the library build stage as parent
# and additionally install Pylint:
RULE base(VERSION, "dev"): lib(VERSION)
RUN pip install pylint

# For the production mode, use Alpine image as parent, 
# and copy compiled binaries from the 'lib' build stage:
RULE base(VERSION, "production"): image(i"python:${PYTHON}-alpine") & \
                                  lib(VERSION) & \
                                  required_python(VERSION, PYTHON)
COPY --from=lib /my_lib /my_lib

# Copy app's source code to the appropriate base image:
RULE app(VERSION, MODE): base(VERSION, MODE)
COPY . /my_app
```

Modus provides a source-to-source translator from Modusfiles to Dockerfiles, `modus-transpile`. In Bash shell, the above build can be executed by running 

    docker build . -f <(modus-transpile Modusfile --query 'app(v"1.2.5", "production")')

where the `--query` option specifies the target image.

Modus can print the proof tree of a given query that shows how the target image is constructed from parent images:

    $ modus-transpile Modusfile --query 'app(v"1.2.5", "release")' --proof
    app(v"1.2.5", "production")
    └── base(v"1.2.5", "production")
        ├── i"python:3.5-alpine"
        ├── lib(v"1.2.5", "production")
        │   ├── i"python:3.5"
        │   ├╶╶ required_python(v"1.2.5", v"3.5")
        │   └╶╶ build_target("production", "debug")
        └╶╶ required_python(v"1.2.5", v"3.5")

Predicates that do not represent images (logic predicates) in the proof tree are preceded with `╶╶`.

## Documentation

- Examples
  - [Avoiding Code Duplication](doc/example-avoiding-code-duplication.md)
  - [Reusing Build Definitions](doc/example-reusing-build-definitions.md)
  - [Improving Build Speed](doc/example-improving-build-speed.md)
  - [Optimising Image Size](doc/example-optimising-image-size.md)
  - [Managing Versions with SemVer Constaints](doc/example-semver-constraints.md)
  - [Build Definitions as Libraries](doc/example-modus-libraries.md)
- Manual
  - [Installation instructions](doc/manual-installation.md)
  - [Modusfile reference](doc/manual-modusfile-reference.md)
  - [Command-line tool](doc/manual-command-line-tool.md)
- Development
  - [Conceptual Overview](doc/development-conceptual-overview.md)
  - [Roadmap](doc/development-roadmap.md)
