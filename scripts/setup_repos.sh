#!/bin/sh
set -e
set -x

eval $(opam config env)
opam repo add tezos-opam-repository https://gitlab.com/nomadic-labs/tezos-opam-repository.git
