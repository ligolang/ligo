#!/bin/bash
docker run -v $PWD:$PWD -w $PWD stovelabs/granary-ligo $@