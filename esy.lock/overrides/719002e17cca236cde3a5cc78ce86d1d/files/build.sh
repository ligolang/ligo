#! /bin/sh


cur__os=$1
HOST_FLAGS=

if [ "$cur__os" = "windows" ]; then
    HOST_FLAGS="--host x86_64-w64-mingw32"
fi

find ./ -exec touch -t 200905010101 {} +
export PATH="/usr/x86_64-w64-mingw32/sys-root/mingw/bin:$PATH"
./configure --enable-fat --enable-cxx --prefix=$cur__install $HOST_FLAGS --with-pic
