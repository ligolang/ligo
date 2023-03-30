#!/usr/bin/env bash

branch_name=$1
ligo_precedent_version=$2
ligo_new_version=$3
token_github_api=$4

pr_name="draft: ligo: $ligo_precedent_version -> $ligo_new_version"

pr_description=$(cat $(dirname "${BASH_SOURCE[0]}")/pr_description.md)

curl --location --request POST "https://api.github.com/repos/NixOS/nixpkgs/pulls" \
    --header "Authorization: Bearer $token_github_api" \
    --header "Content-Type: application/json" \
    --data-raw "{
    \"title\": \"$pr_name\",
    \"head\": \"$branch_name\",
    \"base\": \"master\",
    \"body\": \"$pr_description\",
    \"draft\": true,
    \"reviewers\": [\"ulrikstrid\"]
}"
