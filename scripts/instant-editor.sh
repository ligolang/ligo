#!/usr/bin/env bash
set -euEo pipefail

if [ $# -eq 1 ]; then
    if [ "$1" == "emacs" ]; then
        shift;
        nix-shell shell.nix --run "nix-shell tools/instant-editor/emacs.nix --run \"emacs '${1//"'"/"'\\''"}'\"";
    fi
elif [ $# -eq 0 ]; then
    nix-shell shell.nix;
else
  echo "Usage: ./instant-editor.sh [file]"
  exit 1
fi
