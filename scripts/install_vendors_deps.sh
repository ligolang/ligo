#!/bin/sh
set -e
set -x

# NEW-PROTOCOL-TEMPORARY
sed -i 's/"ppx_inline_test" {with-test}/"ppx_inline_test"/' vendors/tezos/src/lib_stdlib/tezos-stdlib.opam
sed -i 's/"tezos-stdlib"/"tezos-stdlib"\n  "bls12-381-legacy"/' vendors/tezos/src/lib_protocol_environment/tezos-protocol-environment-structs.opam
sed -i 's/{ >= "2.7.2" }/{ >= "2.7.2" \& < "2.8.0" }/' vendors/tezos/src/lib_context/tezos-context.opam
opam update
# NEW-PROTOCOL-TEMPORARY

# Install local dependencies
opam install -y --deps-only --with-test --locked ./ligo.opam
