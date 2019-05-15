#!/bin/bash
set -euET -o pipefail
main(){
  root_dir="$(pwd | sed -e 's/\\/\\\\/' | sed -e 's/&/\\\&/' | sed -e 's/~/\\~/')"
  cd vendors/ligo-opam-repository
  git grep -z -l src: | grep -z 'opam$' | xargs -0 \
    sed -i -e 's~src:  *"https://gitlab.com/gabriel.alfour/ligo/-/archive/master/ligo.tar.gz"~src: "file://'"$root_dir"'"~' \
           -e 's~src:  *"https://gitlab.com/gabriel.alfour/ligo-utils/-/archive/master/ligo-utils.tar.gz"~src: "file://'"$root_dir"'/vendors/ligo-utils"~' \
           -e 's~src:  *"https://gitlab.com/gabriel.alfour/tezos-modded/-/archive/master/tezos-modded.tar.gz"~src: "file://'"$root_dir"'/vendors/tezos-modded"~'
}
if main; then exit 0; else exit $?; fi
