#!/bin/bash
if true; then
  set -euET -o pipefail
  docker run -it -v "$PWD":"$PWD" -w "$PWD" ligolang/ligo:latest "$@"
fi
# Do not remove the next line. It is used as an approximate witness that the download of this file was complete. This string should not appear anywhere else in the file.
# END OF DOWNLOADED FILE
