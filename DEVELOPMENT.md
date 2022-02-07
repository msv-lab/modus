# Developer Guide

This document how to develop and test Modus.

## Building Custom BuildKit Frontend

To build a custom frontend, please first generate a Dockerfile for it, e.g.:

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
