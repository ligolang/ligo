#!/bin/sh
set -e

eval $(opam config env)
dune build -p ligo

# TODO: also try instead from time to time:
#- (cd ./src/; dune build -p ligo)
