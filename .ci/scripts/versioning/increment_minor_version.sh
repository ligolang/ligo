#!/usr/bin/env bash
VERSION_TO_INCREMENT=$1

VERSION_REGEX="([0-9]+)\.([0-9]+)\.([0-9]+)"

 if [[ $VERSION_TO_INCREMENT =~ $VERSION_REGEX ]]; then
    MAJOR=${BASH_REMATCH[1]}
    MINOR=${BASH_REMATCH[2]} 
    FIX=${BASH_REMATCH[3]}
  else
    echo "no match found, the input version is not formatted with X.Y.Z"
  fi

echo $MAJOR.$(($MINOR+1)).$FIX
