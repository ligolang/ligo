#!/bin/sh

# TODO this is exactly like install_vendors_deps.sh but doesn't
# export cargo bins path

set -e
set -x

# Install local dependencies
export PATH=~/.cargo/bin:$PATH

OPAMSOLVERTIMEOUT=600 OPAMSOLVERTOLERANCE=0.0003 BLST_PORTABLE=y opam install -y --deps-only --with-test --with-doc .

# TODO: this is a hack
cd vendors
git clone https://gitlab.com/ligolang/tezos-ligo.git
cd tezos-ligo
git checkout 4d1f2bc8cdc13690328ead815dae7219561b38e5
cd ../..
