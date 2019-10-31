#!/bin/sh
set -e
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
        rsync

else
    apt-get update -qq
    apt-get -y -qq install \
        libev-dev \
        perl \
        pkg-config \
        libgmp-dev \
        libhidapi-dev \
        m4 \
        libcap-dev \
        bubblewrap \
        rsync
fi
