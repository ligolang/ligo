#!/usr/bin/env bash

# Calculates per-file diff for each file in directory
shopt -s globstar;
set -e;
PW2=$(readlink -e $2)

pushd $1;
for F in **/*; do
    echo diff $F $PW2/$F;
    diff $F $PW2/$F;
done
popd
