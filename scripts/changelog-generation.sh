#!/usr/bin/env nix-shell
#!nix-shell -p mustache-go jq  -i bash

# shellcheck shell=bash

cd "$(dirname "${BASH_SOURCE[0]}"/..)"

./scripts/changelog-json.sh > changelog.json

# For html rendering, we want to replace <> which can be interpreted as html by &lt; &gt; except for <p> and </p>
# That's a dirty fix, but didn't find another one...
cat changelog.json | sed "s#<#\&lt;#g" | sed "s#>#\&gt;#g"  | sed "s#\&lt;p\&gt;##g"  | sed "s#\&lt;\/p\&gt;##g" > changelog-for-web-rendering.json

mustache ./changelog-for-web-rendering.json ./scripts/changelog.md.mustache > changelog.md
mustache ./changelog.json ./scripts/changelog.txt.mustache > changelog.txt

jq '.changelog[0]' changelog.json > release-notes.json

mustache ./release-notes.json ./scripts/release-notes.md.mustache > release-notes.md
mustache ./release-notes.json ./scripts/release-notes.txt.mustache > release-notes.txt

