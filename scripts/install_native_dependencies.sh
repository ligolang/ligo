#!/bin/sh
set -e
set -x
. /etc/os-release

if [ $ID = arch ]
then
    pacman -Sy
    sudo pacman -S --noconfirm \
        libevdev \
        perl \
        pkg-config \
        gmp \
        hidapi \
        m4 \
        libcap \
        bubblewrap \
        rsync \
        git
elif [ $ID = nixos ]
then
    echo "Run the following command to be in a good shell"
    echo "nix-shell -p opam -p pkg-config -p cargo -p rustc -p libffi -p gmp -p cmake -p libev -p python3 -p python310Packages.jsonschema"
else
    apt-get update -qq
    apt-get -y -qq install \
        jsonschema \
        libev-dev \
        perl \
        pkg-config \
        libgmp-dev \
        libhidapi-dev \
        m4 \
        libcap-dev \
        bubblewrap \
        rsync \
        git
fi
