# shellcheck shell=bash

set -euET -o pipefail

cd "$( dirname "${BASH_SOURCE[0]}" )"/../gitlab-pages/docs/manpages

opam exec -- dune exec -- ligo --help | perl -pe "s/\\\\N'39'/chr(96)/ge" | perl -pe "s/\\\\N'(\d+)'/chr(\$1)/ge" | pandoc -f man -o ligo.md --shift-heading-level-by=2

for SUBCOMMAND in "run test" "info measure-contract" "compile contract" "compile parameter" "compile storage" "compile expression" "transpile contract" "transpile expression" "run interpret" "run dry-run" "run evaluate-call" "run evaluate-expr" "changelog" "print dependency-graph" "print cst" "print ast-imperative" "print ast-core" "print ast-typed" "print ast-combined" "print mini-c" "info list-declarations" "print preprocessed" "print pretty" "info get-scope" "install"; do
  opam exec -- dune exec -- ligo $SUBCOMMAND --help | perl -pe "s/\\\\N'39'/chr(96)/ge" | perl -pe "s/\\\\N'(\d+)'/chr(\$1)/ge" | pandoc -f man -o "$SUBCOMMAND.md" --shift-heading-level-by=2;
done
