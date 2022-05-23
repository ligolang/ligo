#!/bin/sh
set -e

"$(dirname "$0")"/setup_switch.sh

opam install -y ocp-indent tuareg merlin alcotest-lwt crowbar ocaml-lsp-server ocamlformat-rpc
opam -y user-setup install
