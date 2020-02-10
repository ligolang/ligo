#!/bin/sh
set -e
set -x
docker build -t "${LIGO_REGISTRY_IMAGE_BUILD:-ligolang/ligo}:next" -f ./docker/distribution/debian/distribute.Dockerfile .
