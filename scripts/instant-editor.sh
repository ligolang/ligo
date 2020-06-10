#!/usr/bin/env bash
set -euEo pipefail

if [ $# -eq 2 ]; then
    nix-shell shell.nix --run "nix-shell tools/instant-editor/emacs.nix --run \"$1 '${2//"'"/"'\\''"}'\"";
elif [ $# -eq 0 ]; then
    nix-shell shell.nix;
else
  echo "Usage: ./scripts/instant-editor.sh [editor file]"
  exit 1
fi
