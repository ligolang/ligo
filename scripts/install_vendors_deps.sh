#!/bin/sh
set -e
set -x

# Install local dependencies
opam install -y --deps-only --with-test --locked ./ligo.opam $(find vendors -name \*.opam)
opam install -y $(find vendors -name \*.opam)
