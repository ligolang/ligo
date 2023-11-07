#!/bin/sh
set -e
set -x

if [ -z "${LIGO_JOBS}" ]; then
    JOBS=""
else
    JOBS="-j ${LIGO_JOBS}"
    export OPAM_JOBS="${LIGO_JOBS}"
fi

opam exec -- dune build -p ligo $JOBS

# TODO: also try instead from time to time:
#- (cd ./src/; dune build -p ligo)
