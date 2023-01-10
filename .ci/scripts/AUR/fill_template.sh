#!/usr/bin/env bash
cd "$(dirname "${BASH_SOURCE[0]}")"

TEMPLATE_FILEPATH=$1
DEB_PKG_URL=$2
CURRENT_VERSION=$3

SED_IN_PLACE_COMMAND=(sed -i)
if [ "$(uname)" == "Darwin" ]; then
    # Sed is different in macos. https://stackoverflow.com/questions/7573368/in-place-edits-with-sed-on-os-x
    SED_IN_PLACE_COMMAND=(sed -i '')
fi

"${SED_IN_PLACE_COMMAND[@]}" -E "s#LIGO_VERSION_PLACEHOLDER#$CURRENT_VERSION#g" "$TEMPLATE_FILEPATH"
"${SED_IN_PLACE_COMMAND[@]}" -E "s#DEB_PKG_URL_PLACEHOLDER#$DEB_PKG_URL#g" "$TEMPLATE_FILEPATH"
