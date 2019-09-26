#!/bin/sh
set -e
set -x

eval $(opam config env)

# Remove the nomadic-labs tezos repo (from ligo switch only)
opam repository remove tezos-opam-repository

# Add ligolang tezos repo
opam repository add ligolang-tezos-opam-repository https://gitlab.com/ligolang/tezos-opam-repository.git
