#!/usr/bin/env bash
cd "$(dirname "${BASH_SOURCE[0]}")"

TEMPLATE_FILEPATH=$1
LAST_TAG_JOB_ID=$2
CURRENT_VERSION=$3
GIT_SHA_REFERENCE=$4

SED_IN_PLACE_COMMAND=(sed -i)
if [ "$(uname)" == "Darwin" ]; then
    # Sed is different in macos. https://stackoverflow.com/questions/7573368/in-place-edits-with-sed-on-os-x 
    SED_IN_PLACE_COMMAND=(sed -i '')
fi

"${SED_IN_PLACE_COMMAND[@]}" -E "s#LIGO_VERSION_PLACEHOLDER#$CURRENT_VERSION#g" "$TEMPLATE_FILEPATH"
"${SED_IN_PLACE_COMMAND[@]}" -E "s#LIGO_HEAD_REF_HASH_PLACEHOLDER#$GIT_SHA_REFERENCE#g" "$TEMPLATE_FILEPATH"
"${SED_IN_PLACE_COMMAND[@]}" -E "s#LIGO_ARTIFACT_JOB_ID_PLACEHOLDER#$LAST_TAG_JOB_ID#g" "$TEMPLATE_FILEPATH"
