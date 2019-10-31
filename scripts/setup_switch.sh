#!/bin/sh
set -e
set -x

printf '' | opam switch create . ocaml-base-compiler.4.07.1 # toto ocaml-base-compiler.4.06.1
eval $(opam config env)
