#!/bin/bash
set -euET -o pipefail
main(){
  root_dir="$(pwd | sed -e 's/\\/\\\\/' | sed -e 's/&/\\\&/' | sed -e 's/~/\\~/')"
  rm -fr vendors/ligo-opam-repository-local-generated
  mkdir vendors/ligo-opam-repository-local-generated
  cp -a index.tar.gz packages repo urls.txt vendors/ligo-opam-repository-local-generated
  cd vendors/ligo-opam-repository-local-generated
  grep -r --null -l src: | grep -z 'opam$' | tr '\0' '\n'
  grep -r --null -l src: | grep -z 'opam$' | xargs -0 \
    sed -i -e 's~src:  *"https://gitlab.com/ligolang/ligo/-/archive/master/ligo\.tar\.gz"~src: "file://'"$root_dir"'"~'
  # TODO: run the update.sh script adequately to regenerate the index.tar.gz etc. in the local repo
}
if main; then exit 0; else exit $?; fi
