#!/bin/sh
set -e
set -x

opam update

# Install local dependencies
BLST_PORTABLE=y opam install -y --deps-only --with-test . --locked
