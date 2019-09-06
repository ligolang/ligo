#!/bin/sh
set -e
set -x

printf '' | opam switch create . 4.07.1 # toto ocaml-base-compiler.4.06.1
eval $(opam config env)

# Add Tezos opam repository
opam repo add tezos-opam-repository https://gitlab.com/nomadic-labs/tezos-opam-repository.git

# TODO: move this to install_vendor_deps.sh
# Pin the versions of some dependencies
opam pin -y zarith 1.7
opam pin -y ipaddr 3.1.0
opam pin -y macaddr 3.1.0
