#!/usr/bin/env nix-shell
#!nix-shell -p jq -i bash
ESY_FILEPATH=$1
VERSION=$2

SED_IN_PLACE_COMMAND="sed -i "
if [ "$(uname)" == "Darwin" ]; then
    # Sed is different in macos. https://stackoverflow.com/questions/7573368/in-place-edits-with-sed-on-os-x 
    SED_IN_PLACE_COMMAND="sed -i ''"
fi

if [ -n "$VERSION" ]; then 
  jq --arg version "$VERSION" '.version = $version' $ESY_FILEPATH > $ESY_FILEPATH.tmp && mv $ESY_FILEPATH.tmp $ESY_FILEPATH
  VERSION_WITHOUT_DEV="$(echo "$VERSION" | sed 's/-dev.*//g')"
  jq --arg version "$VERSION_WITHOUT_DEV" '.override.buildEnv.LIGO_VERSION = $version' $ESY_FILEPATH > $ESY_FILEPATH.tmp && mv $ESY_FILEPATH.tmp $ESY_FILEPATH
fi
