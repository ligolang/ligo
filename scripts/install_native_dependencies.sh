#!/bin/sh
set -e
set -x
. /etc/os-release

if [ $ID = arch ]
then
    pacman -Sy
    sudo pacman -S --noconfirm \
        rakudo \
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

else
    apt-get update -qq
    apt-get -y -qq install \
        perl6 \
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
