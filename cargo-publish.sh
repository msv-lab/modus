#!/usr/bin/env bash

set -xe

COMMIT_HASH=$(git rev-parse HEAD)
SHORT_HASH=${COMMIT_HASH:0:10}
if ! docker manifest inspect "ghcr.io/modus-continens/modus-buildkit-frontend:$SHORT_HASH"; then
  echo "Frontend image does not exist on ghcr - did the CI run completed successfully?"
  exit 1
fi

cargo publish -p modus-lib
sleep 5 # otherwise cargo will not be aware of the newly published modus-lib on crates.io
cargo publish -p modus
