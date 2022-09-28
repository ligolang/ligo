#!/usr/bin/env bash

# `./test_md.sh 1-6` execute Markdown tests number 1 to 6
# `./test_md.sh 4` execute Markdown tests number 4


eval $(opam env)
# trick to copy current .md files into _build
dune build @md_update

pushd _build/default/src/test/ > /dev/null
  ./doc_test.exe test Markdown $1
popd > /dev/null