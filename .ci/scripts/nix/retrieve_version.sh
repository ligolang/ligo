#!/usr/bin/env bash

NIX_FILE=$1

VERSION_REGEX_PATTERN="version = \"([0-9]+\.[0-9]+\.[0-9]+)\";"

sed -rn "s/$VERSION_REGEX_PATTERN/\1/p" $NIX_FILE | tr -d ' '
