#!/bin/sh

# TODO this is exactly like install_vendors_deps.sh but doesn't
# export cargo bins path

set -e
set -x

# Install local dependencies
export PATH=~/.cargo/bin:$PATH

BLST_PORTABLE=y opam install -y --deps-only --with-test . --locked
