#!/bin/sh
set -e
docker build --build-arg target="4.07" -t "${LIGO_REGISTRY_IMAGE_BUILD:-ligolang/ligo}:next" -f ./docker/distribution/generic/build.Dockerfile .
