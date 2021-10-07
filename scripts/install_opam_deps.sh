#!/bin/sh

# TODO this is exactly like install_vendors_deps.sh but doesn't
# install the vendored libs

set -e
set -x

# Install local dependencies
export PATH=~/.cargo/bin:$PATH
opam install -y --deps-only --with-test --locked=locked ./ligo.opam
