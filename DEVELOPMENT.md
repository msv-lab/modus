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

The GitHub Actions workflow handles building, testing and making a draft release on GitHub, but the following steps need to be done manually:

1. Increment the version number in `modus/Cargo.toml` and `modus-lib/Cargo.toml`. Note that the `modus-lib` dependency in `modus/Cargo.toml` also need to have its version incremented.
2. Commit these changes, then create a new git tag pointing to the version bumping commit with the next version number, e.g.:

        git tag 0.1.2

    and push the tag to trigger the workflow:

        git push && git push --tags

3. Once the workflow is complete, publish the source code to crates.io with:

        ./cargo-publish.sh

4. The workflow will only create a draft release on GitHub. Go to [the Releases page](https://github.com/modus-continens/modus/releases) and publish the draft.
