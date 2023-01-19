#!/usr/bin/env nix-shell
#!nix-shell -p git dpkg -i bash

# shellcheck shell=bash

#!/bin/bash
cd "$( dirname "${BASH_SOURCE[0]}" )"/..

LIGO_BINARY_GITLAB_CI_PATH="./ligo"
LIGO_BINARY_PATH=${1:-$LIGO_BINARY_GITLAB_CI_PATH}

determine_version () {
  version_regex='[0-9]+\.[0-9]+\.[0-9]+'
  if [[ "$CI_COMMIT_TAG" =~ $version_regex ]]; then
    version=$CI_COMMIT_TAG
  else
    version="0.0.0-next"
  fi
  if [[ $CI_COMMIT_SHA ]]; then
    revision="$CI_COMMIT_SHA"
  else
    revision=`git rev-parse HEAD`
  fi
}

determine_version

# Preparing control file
cat > control << EOF
Package: ligo
Version: ${version}-${revision}
Priority: optional
Architecture: amd64
Maintainer: ligolang ligolang.org
Description: ligo
  A friendly Smart Contract Language for Tezos
EOF

# Generate working directory
pkgName="ligo_0ubuntu${version}-${revision}_amd64"

mkdir -p $pkgName/usr/local/bin
mkdir -p $pkgName/DEBIAN

cp $LIGO_BINARY_PATH $pkgName/usr/local/bin/ligo
mv ./control $pkgName/DEBIAN/control

# Generate .deb
dpkg-deb -Zxz --build $pkgName
ln $pkgName.deb ligo.deb
