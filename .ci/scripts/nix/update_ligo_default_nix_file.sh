#!/usr/bin/env bash

FILEPATH=$1
VERSION=$2

VERSION_REGEX_PATTERN="version = \"[0-9]+\.[0-9]+\.[0-9]+\";"
NIX_SHA256_SRI_REGEX_PATTERN='(sha256 = ").*(";)'

SED_IN_PLACE_COMMAND=(sed -i)
if [ "$(uname)" == "Darwin" ]; then
  # Sed is different in macos. https://stackoverflow.com/questions/7573368/in-place-edits-with-sed-on-os-x
  SED_IN_PLACE_COMMAND=(sed -i '')
fi

# Replace SRI for nix
SRI_LIGO_BINARY_HASH=$(nix --extra-experimental-features nix-command hash to-sri --type sha256 $(jq -r .sha256 <<< $(nix run nixpkgs#nix-prefetch-git -- --rev $VERSION --url https://gitlab.com/ligolang/ligo.git --fetch-submodules --quiet --no-deepClone)))
echo "update distribution reference SRI_LIGO_BINARY_HASH = $SRI_LIGO_BINARY_HASH"

# Idk why, all nixPkgs files are 644, so I change it temporarly to be able to edit it
chmod 664 $FILEPATH
"${SED_IN_PLACE_COMMAND[@]}" -E "s#$VERSION_REGEX_PATTERN#version = \"$VERSION\";#" "$FILEPATH"
"${SED_IN_PLACE_COMMAND[@]}" -E "s#$NIX_SHA256_SRI_REGEX_PATTERN#\1$SRI_LIGO_BINARY_HASH\2#" "$FILEPATH"
chmod 644 $FILEPATH
