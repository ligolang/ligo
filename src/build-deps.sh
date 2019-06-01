#!/bin/sh
vendors/opam-repository-tools/rewrite-local-opam-repository.sh
opam repository add localrepo "file://$PWD/vendors/ligo-opam-repository-local-generated/"
opam install ocp-indent merlin alcotest-lwt crowbar
opam user-setup install
opam install -y --build-test --deps-only ./src/ 
