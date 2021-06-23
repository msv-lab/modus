# Modus

Modus is a [Datalog](https://en.wikipedia.org/wiki/Datalog)-based [DSL](https://en.wikipedia.org/wiki/Domain-specific_language) for building Docker images. It supports complex build workflows for configurable, evolving software. Modus is declarative and non-Turing-complete. Compared with Dockerfiles, it provides the following extra capabilities:

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

Let's consider a more complex example that showcases some features of Modus. Assume that we would like to containerise the application `app`. Suppose also that `app` depends on the library `library`, different versions of which depend on different versions of Python. We would like to have two build mode: "development" mode for development and testing on different versions of Linux, and "production" mode with a smaller image and better security using Alpine. The Modusfile below defines a parametrised build that (1) automatically resolves dependencies, and (2) supports both "development" and "production" modes without code duplication. 

```Prolog
% Relation between the library version and the required Python version:
library_python(v, "3.6") :- version_lt(v, "1.1.0").
library_python(v, "3.7") :- version_geq(v, "1.1.0"), version_lt(v, "1.3.0-alpha").
library_python(v, "3.8") :- version_geq(v, "1.3.0-alpha").

% Python installation on different distros:
install_python(image, python) :-
  	image_repo(image, "fedora"),
	run(f"dnf install python${python}").
install_python(image, python) :-
  	image_repo(image, "ubuntu"),
	image_tag(image, tag),
	version_geq(tag, "16.04"),
  	arg("DEBIAN_FRONTEND", "noninteractive"),
	run(f"apt-get install -y python${python}").
install_python(image, python) :-
  	image_repo(image, "ubuntu"),
	image_tag(image, tag),
  	version_lt(tag, "16.04"),
        version_geq(python, "3.7"),
	run(f"apt-get install -y software-properties-common && \
              add-apt-repository ppa:deadsnakes/ppa && \
              apt-get update && \
              apt-get install -y python${python}").

% The build stage that downloads and compiles the library.
% Python's version and the make target are resolved based
% on the library version and the build mode:
build(image, version, mode, output) :-
    library_python(version, python),
    from(image),
    install_python(image, version),
    arg("DEBIAN_FRONTEND", "noninteractive"),
    run("apt-get install -y make"),
    run(f"wget https://library.com/releases/library-v${version}.tar.gz && \
          tar xf library-v${version}.tar.gz && \
          mv library-v${version}/ /library-build"),
    workdir("/library-build"),
    (mode = "development", run("make debug"), output = "/library-build/debug/";
     mode = "production", run("make release"), output = "/library-build/release/").

% In development mode, use the build stage as the parent image,
% and additionally install development tools (Pylint):
dependencies(image, version, "development") :-
    build(image, version, "development", output),
    run(f"cp ${output} /my_lib"),
    run("pip install pylint").

% In production mode, use Alpine as the parent image,
% and copy compiled binaries from the build stage:
dependencies(image, version, "production") :-
    library_python(version, python),
    from(f"python:${python}-alpine"),
    build(image, version, "production", output)::copy(output, "/my_lib").

% Copy app's source code to the appropriate parent image:
app(image, version, mode) :-
    dependencies(image, version, mode),
    copy(".", "/my_app").
```

For a given query, Modus generates a Dockerfile that builds the corresponding targets, using the `modus-transpile` tool. In Bash, the above build can be executed by running 

    docker build . -f <(modus-transpile Modusfile 'app("ubuntu:18.04", "1.2.5", "production")')

Modus can print the build tree of a given target that shows how the target image is constructed from parent images:

    $ modus-transpile Modusfile 'app("ubuntu:18.04", "1.2.5", "production")' --tree
    app("ubuntu:18.04", "1.2.5", "production")
    ╘══ dependencies("ubuntu:18.04", "1.2.5", "production")
        ╞══ from("python:3.7-alpine")
        ├── build("ubuntu:18.04", "1.2.5", "production", "/library_build/release")::copy("/library_build/release", "/my_lib")
        │   ╞══ from("ubuntu:18.04")
        |   ├── install_python("ubuntu:18.04", "3.7")
        │   └── library_python("1.2.5", "3.7")
        └── library_python("1.2.5", "3.7")

Modus can also build multiple images if the target contains a variable:

    $ modus-transpile Modusfile 'app("ubuntu:18.04", "1.2.5", X)' --tree
    app("1.2.5", "production")
    ╘══ dependencies("1.2.5", "production")
        ╞══ from("python:3.7-alpine")
        ├── build("ubuntu:18.04", "1.2.5", "production", "/library_build/release")::copy("/library_build/release", "/my_lib")
        │   ╞══ from("ubuntu:18.04")
        |   ├── install_python("ubuntu:18.04", "3.7")
        │   └── library_python("1.2.5", "3.7")
        └── library_python("1.2.5", "3.7")
    app("1.2.5", "development")
    ╘══ dependencies("1.2.5", "development")
        ╘══ build("ubuntu:18.04", "1.2.5", "development", "/library_build/debug")
            ╞══ from("ubuntu:18.04")
            ├── install_python("ubuntu:18.04", "3.7")
            └── library_python("1.2.5", "3.7")

In these build trees, parent images are preceded with `══`.

## Documentation

- [Installation](doc/installation.md)
- [Tutorial](doc/tutorial.md)
- [Language reference](doc/language-reference.md)
- [Command-line tool](doc/command-line-tool.md)
- Examples
  - [Optimising image size](doc/example/optimising-image-size.md)
