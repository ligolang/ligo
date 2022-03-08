#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE[0]}")"

KEY=$1

echo `yq ".$KEY" ../../.metadata.yml| tr -d '"'`
