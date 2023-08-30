#!/bin/bash

if [ "$CI_PIPELINE_SOURCE" = "merge_request_event" ]; then
    sed -i '/^### TAG_REMOVE_IN_CASE_OF_MR ###$/,/^### TAG_REMOVE_IN_CASE_OF_MR ###$/d' Dockerfile
    echo "Removed lines between TAG_REMOVE_IN_CASE_OF_MR"
else
    echo "Skipping removal of lines"
fi
