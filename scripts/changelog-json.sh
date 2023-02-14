#!/usr/bin/env nix-shell
#!nix-shell -p jq yaml2json -i bash

# shellcheck shell=bash

set -euET -o pipefail

cd "$( dirname "${BASH_SOURCE[0]}" )"/..

COMPONENT="compiler"
# default to the historic value compiler
if [ $# -ge 1 ]; then
    COMPONENT=$1
fi

TEMP_VERSION="{}"
mapfile -t VERSIONS < <(git tag | grep -E "^[0-9]+\.[0-9]+\.[0-9]+$" | sort -Vr; git log --format=format:%H | tail -n 1)

PREV_VERSION=HEAD
# if a file is corrupted and doesn't respect format, we don't want to stop de pipeline
set +e
for VERSION in "${VERSIONS[@]}"; do
    if [[ "$(git rev-list -n 1 "$VERSION")" == "$(git rev-list -n 1 "$PREV_VERSION")" ]]; then
        PREV_VERSION="$VERSION"
        continue
    fi
    CHANGES="$(git diff --diff-filter=A --name-only "$VERSION" "$PREV_VERSION" -- changelog/${COMPONENT} | sort -r --general-numeric-sort)"
    export PREV_VERSION
    export name
    if [[ "$PREV_VERSION" == "HEAD" ]]; then
        name="next"
        is_next=true
    else
        name="$PREV_VERSION"
        is_next=false
    fi
    # A Version is composed by list of change and indicator which determine in kind exist or not.  (Necessary for mustache processing)
    # Null_ind can exist in case of corrupted file without type. We want to know if it's occur.
    TEMP_VERSION="""$(jq ".[env.name] = {
        breaking_ind: false, 
        added_ind: false,
        fixed_ind: false,
        changed_ind: false,
        deprecated_ind: false,
        removed_ind: false,
        performance_ind: false,
        internal_ind: false,
        other_ind: false,
        is_next: ${is_next},
        null_ind: false}" <<< "$TEMP_VERSION")"""
    TEMP_CATEGORIES="{}"
    for CHANGE in $CHANGES; do
        CHANGE_JSON_FORMATTED=$(yaml2json < "$CHANGE")
        # Skip in case of empty file.
        if [ ! -z "$CHANGE_JSON_FORMATTED" ] ;then
            TYPE=$(jq .type <<< "$CHANGE_JSON_FORMATTED")
            TYPE_IND=$(jq -r .type <<< "$CHANGE_JSON_FORMATTED")_ind 
            TEMP_CATEGORIES="$(jq ".$TYPE = .$TYPE + [ $CHANGE_JSON_FORMATTED ]"<<< "$TEMP_CATEGORIES")"
            # Here we are updating this indicator to determine if the kind exist. 
            TEMP_VERSION="$(jq ".[env.name] = .[env.name] | .[env.name].${TYPE_IND}= true" <<< "$TEMP_VERSION")"
        fi
    done
    TEMP_VERSION="$(jq ".[env.name] = .[env.name] + ( $TEMP_CATEGORIES )" <<< "$TEMP_VERSION")"
    PREV_VERSION="$VERSION"
done

set -e
sed -E -e 's/\\\\n/\\n/g' -e 's/\\\\//g' <<< $(jq "to_entries | .[] | { version: .key, is_next: .value.is_next ,changes: .value }" <<< "$TEMP_VERSION" | jq -s | jq "{ changelog: . }")
