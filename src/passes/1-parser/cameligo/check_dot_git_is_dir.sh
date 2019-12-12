#!/bin/sh

set -e

if test -d ../../.git; then
  echo true > dot_git_is_dir
else
  echo false > dot_git_is_dir
  cat .git >> dot_git_is_dir
fi
