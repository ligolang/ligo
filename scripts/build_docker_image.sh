#!/bin/bash
set -euET -o pipefail

docker build -t ligolang/ligo -f docker/Dockerfile .
