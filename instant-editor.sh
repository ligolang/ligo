#!/usr/bin/env bash
set -euo pipefail

if [ $# -gt 0 ]; then
    if [ "$1" == "emacs" ]; then
        shift;
        nix-shell shell.nix --run "nix-shell tools/instant-editor/emacs.nix --run \"emacs $@\"";
    fi
else
    nix-shell shell.nix;
fi
