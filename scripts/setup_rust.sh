#!/bin/sh
set -e
set -x

wget https://sh.rustup.rs/rustup-init.sh
chmod +x rustup-init.sh
./rustup-init.sh --profile minimal --default-toolchain 1.64.0 -y
rm ./rustup-init.sh
