#!/usr/bin/env bash

if [ $# -eq 0 ]; then
  echo "VERSION is mandatory"
  exit 1
fi

VERSION=$1

VERSION_UNDERSCORES=${VERSION//./_}

echo "ligo--${VERSION_UNDERSCORES}"
