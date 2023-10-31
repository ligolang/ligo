#! /usr/bin/env bash

# This script is an ugly workaround for https://gitlab.haskell.org/ghc/ghc/-/issues/20168
# which is needed for building the debugger statically.
# TL;DR wrap 'gcc' invocation in order to ensure that '-static' and '-shared' aren't used together.
# If both are present, '-static' is dropped.

# Ugly workaround for https://gcc.gnu.org/wiki/Response_Files
expandResponseFiles() {
  declare -ga params=()
  local arg
  for arg in "$@"; do
    if [[ "$arg" == @* ]]; then
      readarray -t expandedParams < "${arg/@/}"
      expandedParams=("${expandedParams[@]/#\"}")
      expandedParams=("${expandedParams[@]/%\"}")
      params=( "${params[@]}" "${expandedParams[@]}" )
    else
      params+=( "$arg" )
    fi
  done
}

expandResponseFiles "$@"

rspOut=$(mktemp)

cleanup() {
  rm -f "$rspOut"
}
trap cleanup EXIT INT TERM

# check if '-shared' flag is present
hasShared=0
for param in "${params[@]}"; do
  if [[ "$param" == "-shared" ]]; then
    hasShared=1
  fi
done

if [[ "$hasShared" -eq 0 ]]; then
  # if '-shared' is not set, don't modify the params
  newParams=( "${params[@]}" )
else
  # if '-shared' is present, remove '-static' flag
  newParams=()
  for param in "${params[@]}"; do
    if [[ ("$param" != "-static") ]]; then
      newParams+=( "$param" )
    fi
  done
fi

printf "%q\n" "${newParams[@]}" > "$rspOut"
exec /usr/bin/gcc "@$rspOut"
