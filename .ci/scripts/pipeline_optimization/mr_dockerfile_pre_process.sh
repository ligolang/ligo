#!/bin/bash

SED_IN_PLACE_COMMAND=(sed -i)
if [ "$(uname)" == "Darwin" ]; then
    # Sed is different in macos. https://stackoverflow.com/questions/7573368/in-place-edits-with-sed-on-os-x 
    SED_IN_PLACE_COMMAND=(sed -i '')
fi

if [ "$CI_PIPELINE_SOURCE" = "merge_request_event" ]; then
    "${SED_IN_PLACE_COMMAND[@]}" '/^### TAG_REMOVE_IN_CASE_OF_MR ###$/,/^### TAG_REMOVE_IN_CASE_OF_MR ###$/d' Dockerfile
    echo "Removed lines between TAG_REMOVE_IN_CASE_OF_MR"
else
    echo "Skipping removal of TAG_REMOVE_IN_CASE_OF_MR lines"
fi

if [ -n "$LIGO_DOCKER_SKIPTEST" ]; then
    "${SED_IN_PLACE_COMMAND[@]}" '/^### TAG_REMOVE_IN_CASE_OF_SKIPTEST ###$/,/^### TAG_REMOVE_IN_CASE_OF_SKIPTEST ###$/d' Dockerfile
    echo "Removed lines between TAG_REMOVE_IN_CASE_OF_SKIPTEST"
else
    echo "Skipping removal of TAG_REMOVE_IN_CASE_OF_SKIPTEST lines"
fi
