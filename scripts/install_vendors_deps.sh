#!/bin/sh
set -e
set -x

# Install local dependencies
opam install -y --deps-only --with-test --locked ./ligo.opam
