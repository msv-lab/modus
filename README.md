# Modus

Modus is a [Datalog](https://en.wikipedia.org/wiki/Datalog)-based [DSL](https://en.wikipedia.org/wiki/Domain-specific_language) for building Docker images. It supports complex build workflows for evolving configurable software. Modus is declarative and non-Turing-complete. Compared with Dockerfiles, it provides the following extra capabilities:

- modularity and code reuse;
- automatic resolution of dependencies: images, compilations flags, etc;
- constraints over [SemVer](https://semver.org/) versions;
- parallel builds of multiple images;
- distributed caching.

Besides, it addresses usability issues of Dockerfiles, for example:

- [modular](http://www.eecs.qmul.ac.uk/~mmh/AMCM048/abstraction/procedural.html) parametrised [build stages](https://docs.docker.com/develop/develop-images/multistage-build/);
- build stages can return values ([Docker #32100](https://github.com/moby/moby/issues/32100));
- conditional instructions ([Stackoverflow](https://stackoverflow.com/questions/43654656/dockerfile-if-else-condition-with-external-arguments), [Earthly #779](https://github.com/earthly/earthly/issues/779));
- copying from parametrised stages (e.g. [Docker #34482](https://github.com/moby/moby/issues/34482));
- predictable variable expansion ([Docker #2637](https://github.com/moby/moby/issues/2637)).
- multi-line shell commands ([Docker #34423](https://github.com/moby/moby/issues/34423), [Docker #16058](https://github.com/moby/moby/issues/16058));
- user-defined commands ([Earthly #581](https://github.com/earthly/earthly/issues/581)).

Modus uses semantic versioning; until version 1.0 is declared, breaking changes are possible. We welcome bug reports and feature requests submitted through [GitHub Issues](https://github.com/mechtaev/modus/issues).

## Motivating example

Modus is Datalog with domain-specific extensions. A Dockerfile can be translated into Modusfile as shown in the table:

| Dockerfile | Modusfile | 
| - | - |
| <pre><code class="language-Dockerfile">FROM ubuntu:20.04 AS app</code><br><br><code class="language-Dockerfile">RUN apt-get update && \\</code><br><code class="language-Dockerfile">&nbsp;&nbsp;&nbsp;&nbsp;apt-get install build-essential</code><br><code class="language-Dockerfile">COPY . /app</code><br><code class="language-Dockerfile">RUN cd /app && make </code></pre>  | <pre><code class="language-prolog">app :-</code><br><code class="language-prolog">&nbsp;&nbsp;&nbsp;&nbsp;from(i"ubuntu:20.04"),</code><br><code class="language-prolog">&nbsp;&nbsp;&nbsp;&nbsp;run("apt-get update && \\</code><br><code class="language-prolog">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;apt-get install build-essential"),</code><br><code class="language-prolog">&nbsp;&nbsp;&nbsp;&nbsp;copy(".", "/app"),</code><br><code class="language-prolog">&nbsp;&nbsp;&nbsp;&nbsp;run("cd /app && make").</code></pre> |

Let's consider a more complex example that showcases some features of Modus. Assume that we would like to containerise the application `app`. Suppose also that `app` depends on the library `library`, different versions of which depend on different versions of Python. We would like to have two build mode: "development" mode for development and testing, and "production" mode with a smaller image and better security. The Modusfile below defines a parametrised build that (1) automatically resolves dependencies, and (2) supports both "development" and "production" modes without code duplication. 

The example below uses special literals for Docker images, e.g. `i"python:3.8"`, and for SemVer versions, e.g. `v"1.3.0-alpha"`:

```Prolog
% Relation between the library version and the required Python version:
library_python(Version, v"3.4") :- Version < v"1.1.0".
library_python(Version, v"3.5") :- Version >= v"1.1.0", Version < v"1.3.0-alpha".
library_python(Version, v"3.8") :- Version >= v"1.3.0-alpha".

% The build stage that downloads and compiles the library.
% Python's version and the make target are resolved based
% on the library version and the build mode:
build(Version, Mode, Output) :-
    library_python(Version, Python),
    from(i"python:${Python}"),
    arg("DEBIAN_FRONTEND", "noninteractive"),
    run("apt-get install -y make"),
    run(f"wget https://library.com/releases/library-v${Version}.tar.gz && \
          tar xf library-v${Version}.tar.gz && \
          mv library-v${Version}/ /library-build"),
    workdir("/library-build"),
    (Mode = "development", run("make debug"), Output = "/library-build/debug/";
     Mode = "production", run("make release"), Output = "/library-build/release/").

% In development mode, use the build stage as the parent image,
% and additionally install development tools (Pylint):
dependencies(Version, "development") :-
    build(Version, "development", Output),
    run(f"cp ${Output} /my_lib"),
    run("pip install pylint").

% In production mode, use Alpine as the parent image,
% and copy compiled binaries from the build stage:
dependencies(Version, "production") :-
    library_python(Version, Python),
    from(i"python:${Python}-alpine"),
    build(Version, "production", Output)::copy(Output, "/my_lib").

% Copy app's source code to the appropriate parent image:
app(Version, Mode) :-
    dependencies(Version, Mode),
    copy(".", "/my_app").
```

For a given query, Modus generates a Dockerfile that builds the corresponding targets, using the `modus-transpile` tool. In Bash, the above build can be executed by running 

    docker build . -f <(modus-transpile Modusfile 'app(v"1.2.5", "production")')

Modus can print the build tree of a given target that shows how the target image is constructed from parent images:

    $ modus-transpile Modusfile 'app(v"1.2.5", "production")' --tree
    app(v"1.2.5", "production")
    ╘══ dependencies(v"1.2.5", "production")
        ╞══ from(i"python:3.5-alpine")
        ├── build(v"1.2.5", "production", "/library_build/release")
        │   ╞══ from(i"python:3.5")
        │   └╶╶ library_python(v"1.2.5", v"3.5")
        └╶╶ library_python(v"1.2.5", v"3.5")

Modus can also build multiple images if the target contains a variable:

    $ modus-transpile Modusfile 'app(v"1.2.5", X)' --tree
    app(v"1.2.5", "production")
    ╘══ dependencies(v"1.2.5", "production")
        ╞══ from(i"python:3.5-alpine")
        ├── build(v"1.2.5", "production", "/library_build/release")
        │   ╞══ from(i"python:3.5")
        │   └╶╶ library_python(v"1.2.5", v"3.5")
        └╶╶ library_python(v"1.2.5", v"3.5")
    app(v"1.2.5", "development")
    ╘══ dependencies(v"1.2.5", "development")
        ╘══ build(v"1.2.5", "production", "/library_build/debug")
            ╞══ from(i"python:3.5")
            └╶╶ library_python(v"1.2.5", v"3.5")

In these build trees, parent images are preceded with `══`, images from which files are copied are preceded with `──`, and logical predicates are preceded with `╶╶`.

## Documentation

- [Installation](doc/installation.md)
- [Tutorial](doc/tutorial.md)
- [Language reference](doc/language-reference.md)
- [Command-line tool](doc/command-line-tool.md)
- Examples
  - [Optimising image size](doc/example/optimising-image-size.md)
