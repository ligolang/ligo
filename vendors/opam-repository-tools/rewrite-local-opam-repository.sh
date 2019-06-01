#!/bin/sh

# Stop on error.
set -e

# Defensive checks. We're going to remove an entire folder so this script is somewhat dangerous. Better check in advance what can go wrong in the entire execution of the script.
if test -e index.tar.gz && test -e packages && test -e repo && test -e urls.txt; then
  if test -d vendors/; then
   if test -d "$PWD"; then
      if command -v sed   >/dev/null 2>&1 \
      && command -v rm    >/dev/null 2>&1 \
      && command -v mkdir >/dev/null 2>&1 \
      && command -v cp    >/dev/null 2>&1 \
      && command -v find  >/dev/null 2>&1 \
      && command -v xargs >/dev/null 2>&1 \
      && command -v opam  >/dev/null 2>&1; then

      # Escape the current directory, to be used as the replacement part of the sed regular expression
      escaped_project_root="$(printf %s "$PWD" | sed -e 's/\\/\\\\/' | sed -e 's/&/\\\&/' | sed -e 's/~/\\~/')"

      # Recreate vendors/ligo-opam-repository-local-generated which contains a copy of the files related to the opam repository
      rm -fr vendors/ligo-opam-repository-local-generated
      mkdir vendors/ligo-opam-repository-local-generated
      cp -pR index.tar.gz packages repo urls.txt vendors/ligo-opam-repository-local-generated

      # Rewrite the URLs in the opam repository to point to the project root
      (
        cd vendors/ligo-opam-repository-local-generated
        find . -type f -name opam -print0 | xargs -0 sed -i -e 's~src:  *"https://gitlab.com/ligolang/ligo/-/archive/master/ligo\.tar\.gz"~src: "file://'"$escaped_project_root"'"~'
      )

      # Regenerate the index.tar.gz etc. in the local repo
      (
        cd vendors/ligo-opam-repository-local-generated
        opam admin index
        opam admin cache
      )
      else
        echo "One of the following commands is unavailable: sed rm mkdir cp find xargs opam."
        exit 1
      fi
    else
      echo "Unable to access the current directory as indicated by PWD. Was the CWD of the current shell removed?"
      exit 1
    fi

  else
    echo "Cannot find the directory vendors/ in the current directory"
    exit 1
  fi
else
  echo "Cannot find some of the following files in the current directory"
  echo "index.tar.gz packages repo urls.txt"
  exit 1
fi
