#!/usr/bin/env bash
cd "$(dirname "${BASH_SOURCE[0]}")"

[[ $1 =~ ^https?://[^/]+ ]] && HOST="${BASH_REMATCH[0]}/api/v4/projects/"
CI_PROJECT_ID=$2
CI_MERGE_REQUEST_IID=$3
PRIVATE_TOKEN=$4

echo `curl --silent "${HOST}${CI_PROJECT_ID}/merge_requests/${CI_MERGE_REQUEST_IID}" --header "PRIVATE-TOKEN:${PRIVATE_TOKEN}"`
