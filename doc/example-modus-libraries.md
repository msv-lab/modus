# Modus Libraries (WIP)

In `FROM ...`, instead of writing `maven:3.8.1-jdk-11-slim`, what we actually would like to write is

    maven(v"3.8.1", v"11", "buster", true)

or

    maven(MAVEN_VERSION=v"3.8.1", JDK_VERSION=v"11", DISTR="buster", SLIM=true)

We may want to define a wrapper for `maven` image in a library:

    DECL maven(MAVEN_VERSION: SemVer, JDK_VERSION: SemVer, DISTR: String, SLIM: Boolean)
    RULE maven(v"3.8.1", v"11", "buster", true) :- image("maven:3.8.1-jdk-11-slim")

Libraries can also provide generic rules for building packages (input image is parametrised using `PARENT` symbol):

    DECL apt_build_dep(PARENT: String)
    RULE apt_build_dep("ubuntu:20.04") :- ubuntu(v"20.04")
    RULE apt_build_dep("ubuntu:16.04") :- ubuntu(v"16.04")

    DECL build(CLAGS: String, ARG1: String, ARG2: String, PARENT: String)
    RULE build(CLAGS, ARG1, ARG2, PARENT) :- apt_build(CLAGS, ARG1, ARG2, PARENT); dnf_build(CLAGS, ARG1, ARG2, PARENT)

    RULE apt_build(CLAGS, ARG1, ARG2, PARENT) :- apt_build_dep(PARENT)
    RUN apt-get install gcc
    COPY . /tmp
    RUN cd /tmp && wget ... && make -e A=${ARG1} B=${ARG2}

Usage with namespace:

    IMPORT "lib/build.modus" as lib

    RULE lib.apt_build_dep("custom-ubuntu") :- image(v"20.04")
    RUN install gcc
    RUN install z3

    RULE build() :- lib.build("", "", "", "custom-ubuntu")

The are a couple of issues here:

1. If A imports B, B imports C, and B extends a predicate of C, then will this extension be visible in A?
2. How to control public/private better? When do we need declarations?
3. How to precisely control which base images are used in our build without explicitly propagating all variables?
4. When we import a library that does `COPY . /tmp`, what does this `.` refer to?

TODO: check how libraries are imported in Prolog/Souffle.