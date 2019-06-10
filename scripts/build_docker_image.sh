#!/bin/sh
set -e

docker build -t ligolang/ligo -f docker/Dockerfile .
