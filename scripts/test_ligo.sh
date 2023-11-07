#!/bin/sh
set -e

if [ -z "${LIGO_JOBS}" ]; then
    JOBS=""
else
    JOBS="-j ${LIGO_JOBS}"
    export OPAM_JOBS="${LIGO_JOBS}"
fi

eval $(opam config env)
dune runtest $JOBS
