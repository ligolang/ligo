#!/bin/sh
set -e
set -x

printf '' | opam switch create . ocaml-base-compiler.5.2.1 --no-install || true
eval $(opam config env)
