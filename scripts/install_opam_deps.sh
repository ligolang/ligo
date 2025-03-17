#!/bin/sh

# TODO this is exactly like install_vendors_deps.sh but doesn't
# export cargo bins path

set -e
set -x

# Install local dependencies
export PATH=~/.cargo/bin:$PATH

BLST_PORTABLE=y opam install -y --deps-only --with-test --with-doc .

# TODO: this is a hack
cd vendors
git clone https://gitlab.com/ligolang/tezos-ligo.git
cd tezos-ligo
git checkout fb4bad17f4d4a8b1df1ba5ea96935f63321e3a30
cd ../..
