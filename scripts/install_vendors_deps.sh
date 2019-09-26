#!/bin/sh
set -e

# Install local dependencies
opam install -y --deps-only --with-test $(find src vendors -name \*.opam)
opam install -y $(find vendors -name \*.opam)
