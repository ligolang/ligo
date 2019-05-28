#!/bin/sh
set -e

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
