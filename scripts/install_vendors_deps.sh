#!/bin/sh
set -e

# Install local dependencies
opam install -y ./vendors/ligo-utils/simple-utils
opam install -y ./vendors/ligo-utils/tezos-protocol-alpha
opam install -y ./vendors/ligo-utils/tezos-protocol-alpha-parameters
opam install -y ./vendors/ligo-utils/memory-proto-alpha
opam install -y ./vendors/ligo-utils/tezos-utils/michelson-parser
opam install -y ./vendors/ligo-utils/tezos-utils
opam install -y ./vendors/ligo-utils/proto-alpha-utils
opam install -y getopt ppx_deriving menhir
