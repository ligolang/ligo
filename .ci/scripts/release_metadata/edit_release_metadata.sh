#!/usr/bin/env bash
cd "$(dirname "${BASH_SOURCE[0]}")"

KEY=$1
VALUE=$2

yq -i ".$KEY = \"$VALUE\"" ../../.metadata.yml
