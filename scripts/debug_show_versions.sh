#!/bin/sh
set -e

echo "$PATH"
opam --version
printf '' | ocaml
opam switch

