#!/usr/bin/env bash
ESY_FILEPATH=$1
SYSTEM_ARCH=$2
GENERATE_NEW_DEV_VERSION=$3

CURRENT_VERSION=$(jq -r '.version' $ESY_FILEPATH)

if [[ $GENERATE_NEW_DEV_VERSION = true ]]; then 
  # If the goal is only to upgrade minor version for release process (generation of a new dev version to commit).
  INCREMENT_VERSION_SCRIPT="$(dirname "${BASH_SOURCE[0]}")/increment_minor_version.sh"
  VERSION_WITHOUT_DEV="$(echo "$CURRENT_VERSION" | sed 's/-dev.*//g')"
  NEW_VERSION=$(bash $INCREMENT_VERSION_SCRIPT $VERSION_WITHOUT_DEV)
  echo "$NEW_VERSION-dev"
else
  if [[ ! $SYSTEM_ARCH =~ ^(macos-m1|macos-intel|linux|windows)$ ]]; then
    echo "Invalid value for SYSTEM_ARCH: $SYSTEM_ARCH. Valid values are: macos-m1, macos-intel, linux, windows."
    exit 1
  fi
  # Starting by check if gitlab CI corresponding to the current tag is defined.
  if [ -n "$CI_COMMIT_TAG"  ]; then
    # If yes the next version is the one provided by gitlab to generate a release version to publish
    echo "$CI_COMMIT_TAG.$SYSTEM_ARCH.$(git rev-parse HEAD)"
    exit 0
  fi
  # Here we want to generate a dev version to publish
  echo "$CURRENT_VERSION.$SYSTEM_ARCH.$(git rev-parse HEAD)"
fi
