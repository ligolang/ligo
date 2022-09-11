#!/usr/bin/env bash
cd "$(dirname "${BASH_SOURCE[0]}")"

[[ $1 =~ ^https?://[^/]+ ]] && HOST="${BASH_REMATCH[0]}/api/v4/projects/"
CI_PROJECT_ID=$2
TAG=$3

echo $(curl -s "${HOST}${CI_PROJECT_ID}/repository/commits/${TAG}")
