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
BLST_PORTABLE=y opam install -y --deps-only --with-test . --locked
