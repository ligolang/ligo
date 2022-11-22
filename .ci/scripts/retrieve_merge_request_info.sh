#!/usr/bin/env bash

HOST=$1
CI_PROJECT_ID=$2
CI_MERGE_REQUEST_IID=$3
PRIVATE_TOKEN=$4

echo `python3 parse_mr_info.py --hostname "${HOST}" --project "${CI_PROJECT_ID}" --mr-id "${CI_MERGE_REQUEST_IID}" --token "${PRIVATE_TOKEN}"`
