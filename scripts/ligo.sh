#!/bin/sh
set -e
if test "x$PWD" = "x"; then
  echo "Cannot detect the current directory, the environment variable PWD is empty."
  exit 1
else
  docker run --rm -it -v "$PWD":"$PWD" -w "$PWD" ligolang/ligo:next "$@"
fi
# Do not remove the next line. It is used as an approximate witness that the download of this file was complete. This string should not appear anywhere else in the file.
# END OF DOWNLOADED FILE
