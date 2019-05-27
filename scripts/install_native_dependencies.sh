#!/bin/bash
set -euET -o pipefail

apt-get -y install \
    libev-dev \
    perl \
    pkg-config \
    libgmp-dev \
    libhidapi-dev \
    m4
