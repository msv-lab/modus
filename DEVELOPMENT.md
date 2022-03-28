# Developer Guide

This document explains how to develop and test Modus.

## Building Custom BuildKit Frontend

To build a custom BuildKit frontend, please first generate a Dockerfile for it, e.g.:

    ./target/debug/modus transpile Modusfile 'all("frontend", "release")' > /tmp/modus-frontend.Dockerfile

After that, build a frontend image:

    docker build -f /tmp/modus-frontend.Dockerfile . -t my-buildkit-frontend

The frontend can be enabled by passing its name to `modus build` using the option `--custom-buildkit-frontend`:

    modus build ... --custom-buildkit-frontend my-buildkit-frontend


## Executing Integration Tests

To run integration tests, use the following command:

    cd test
    python -m unittest discover

Use the environment variable `MODUS_EXECUTABLE` to specify the modus executable (`modus` by default), and `MODUS_BUILDKIT_FRONTEND` to specify custom BuildKit frontend (`None` by default).

## Making a New Release

All building, tests and release making are handled by the GitHub Actions workflow except for `cargo publish`. To publish a new release, simply create a new git tag with the next version number, e.g.:

    git tag 0.1.2

and push the tag to trigger the workflow:

    git push --tags

Once the workflow is complete, publish the source code to crates.io with:

    ./cargo-publish.sh
