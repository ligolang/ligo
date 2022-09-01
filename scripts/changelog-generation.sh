#!/usr/bin/env nix-shell
#!nix-shell -p mustache-go jq  -i bash

# shellcheck shell=bash

cd "$(dirname "${BASH_SOURCE[0]}"/..)"

./scripts/changelog-json.sh > changelog.json

mustache ./changelog.json ./scripts/changelog.md.mustache > changelog.md
mustache ./changelog.json ./scripts/changelog.txt.mustache > changelog.txt

jq '.changelog[0]' changelog.json > release-notes.json

mustache ./release-notes.json ./scripts/release-notes.md.mustache > release-notes.md
mustache ./release-notes.json ./scripts/release-notes.txt.mustache > release-notes.txt
