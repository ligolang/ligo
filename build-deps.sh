#!/bin/sh
opam switch create .. ocaml-base-compiler.4.06.1
eval $(opam env)
vendors/opam-repository-tools/rewrite-local-opam-repository.sh
opam repository add localrepo "file://$PWD/vendors/ligo-opam-repository-local-generated/"
opam install -y ocp-indent merlin alcotest-lwt crowbar
opam -y user-setup install
opam install -y --build-test --deps-only ./src/ 
