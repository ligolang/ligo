#!/usr/bin/env bash

test () {
  # remove ./ in file path and run expect_tests
  eval $(opam env)
  EXPECT_ONLY_TEST=$(echo $1 | awk '{print substr($1, 3, length($1))}') dune runtest src/bin/expect_tests
}

list () {
  pushd src/bin/expect_tests > /dev/null
    # get all expect tests locations matching on ocaml let extension
    x=$(grep --line-number --binary-files=without-match -rGT --exclude-dir=".formatted" "let%expect_test" .)
    # remove the last ':' after the line number in x
    echo "$x" | awk '{print $1substr($2, 1, length($2)-1)}'
  popd > /dev/null
}

case "$1" in
  ("")
    echo "Usage:
      !!!! First, uncomment the only-test line in src/bin/expect_tests/dune !!!! and never commit it :)

      ./test_expect.sh list     list of all expect tests 'ID -> file_path_from_root:line_number'
      ./test_expect.sh 42       execute test number 42, use 'dune promote' to promote the new behavior if any

      to run all the tests, use 'dune runtest src/bin/expect_tests' (unfortunately do not stop on first error..)
      Note: if you add new tests, you might have to rerun 'test_expect list'"
    ;;
  ("list")
    # print the list of tests prepending the path to expect_tests from root
    list | awk '{print NR" -> src/bin/expect_tests/"$1}'
    ;;
  (*)
    file=$(list | sed -n "$1"p)
    test $file
    ;;
esac