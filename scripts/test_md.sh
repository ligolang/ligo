#!/usr/bin/env bash

test () {
  pushd _build/default/src/test/ > /dev/null
    ./doc_test.exe $1
  popd > /dev/null
}

eval $(opam env)

# this is 2X slower, enven if no changes in the code . . .
  # dune build src/test/doc_test.exe
# trick to copy current .md files into _build and build faster
dune build @md_update

case "$1" in
  ("")
    echo "Usage:
      make sure you run 'dune build src/test/doc_test.exe' once
      then you can make mutiple call to this script without having
      to rebuild anything as long as you only change the .md files

      ./test_md.sh list list of all Markdown tests
      ./test_md.sh 1-6  execute Markdown tests number 1 to 6
      ./test_md.sh 4    execute Markdown tests number 4"
    ;;
  ("list") test "list";;
  (*)      test "test Markdown $1" ;;
esac