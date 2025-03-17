#!/bin/sh
set -e
set -x

if [ -z "${LIGO_JOBS}" ]; then
    JOBS=""
else
    JOBS="-j ${LIGO_JOBS}"
    export OPAM_JOBS="${LIGO_JOBS}"
fi

opam update

# Install local dependencies
OPAMSOLVERTIMEOUT=600 OPAMSOLVERTOLERANCE=0.0003 BLST_PORTABLE=y opam install -y --deps-only --with-test .

# TODO: this is a hack
cd vendors
git clone https://gitlab.com/ligolang/tezos-ligo.git
cd tezos-ligo
git checkout fb4bad17f4d4a8b1df1ba5ea96935f63321e3a30
cd ../..
