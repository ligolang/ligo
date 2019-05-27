#!/bin/bash
set -euET -o pipefail

cd src && opam install . --yes
