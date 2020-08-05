#!/bin/sh
set -e
set -x

printf '' | opam switch create . ocaml-base-compiler.4.09.1 --no-install
eval $(opam config env)
