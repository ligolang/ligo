#!/bin/sh
set -e

# Install local dependencies
opam install -y --deps-only --with-test ./ligo.opam $(find vendors -name \*.opam)
opam install -y $(find vendors -name \*.opam)
