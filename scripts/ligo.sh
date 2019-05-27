#!/bin/bash
docker run -it -v "$PWD":"$PWD" -w "$PWD" ligolang/ligo "$@"