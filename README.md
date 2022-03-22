[![Crates.io Version](https://img.shields.io/crates/v/modus.svg)](https://crates.io/crates/modus)
[![Crates.io Licence](https://img.shields.io/crates/l/modus.svg)](https://crates.io/crates/modus)

Modus is a language for building OCI/Docker container images. Modus uses logic programming to solve key pain points of Dockerfiles - no way to express interaction among parameters, inability to specify complex build workflows, difficult and inefficient parallelisation, bloated images, and costly maintenance. For more information, please follow these links:

- [Modus website](https://modus-continens.com)
- [Documentation](https://docs.modus-continens.com)
- [Playground](https://play.modus-continens.com)
- [Discord community](https://discord.gg/bXxwfVE9Kj)

Modus uses semantic versioning; until version 1.0 is declared, breaking changes are possible. The current version, 0.1, is a preview release. We welcome bug reports and feature requests submitted through [GitHub Issues](https://github.com/mechtaev/modus/issues).

# Installation

Building Modus requires the [latest stable version of Rust](https://www.whatrustisit.com/), although
versions 1.59+ may also work.

``` sh
cargo install modus
```
or 
``` sh
git clone https://github.com/modus-continens/modus.git
cargo install --path ./modus --profile release
```

Going forward, there will be pre-built executables for major platforms available with every Modus release.

## Development - Custom Buildkit Frontend

For development purposes, you may wish to use a `--custom-buildkit-frontend` to `modus build` an image.
Use your `modus` installation from above and follow the instructions [here](./DEVELOPMENT.md).
