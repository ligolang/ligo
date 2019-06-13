#!/bin/sh
set -e

# TODO: this has many different modes of failure (file temp.opam-2.0.1-x86_64-linux.download-in-progress already exists, /usr/local/bin/opam already exists and is a directory or hard link, …)
# Try to improve these aspects.

wget https://github.com/ocaml/opam/releases/download/2.0.1/opam-2.0.1-x86_64-linux -O temp.opam-2.0.1-x86_64-linux.download-in-progress
cp -i temp.opam-2.0.1-x86_64-linux.download-in-progress /usr/local/bin/opam
chmod +x /usr/local/bin/opam
rm temp.opam-2.0.1-x86_64-linux.download-in-progress
