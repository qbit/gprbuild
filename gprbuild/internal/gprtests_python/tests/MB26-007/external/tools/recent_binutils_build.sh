#!/bin/bash

# This script creates the binutils subdirectory and populates it with only addr2line and objcopy

set -e

BINUTILS_VER=2.21.1

echo "If the wget fails, export the proxy's address in HTTP_PROXY"
mkdir -p binutils
cd    binutils
wget -N "http://ftp.gnu.org/gnu/binutils/binutils-${BINUTILS_VER}.tar.bz2"
tar -xvjf binutils-${BINUTILS_VER}.tar.bz2
cd binutils-${BINUTILS_VER}
CFLAGS='-march=x86-64 -O3 -pipe' ./configure --prefix=`pwd`/..
make -j8
make install
cd ..
mv bin/objcopy bin/addr2line .
find . -mindepth 1 -type d | xargs rm -rf
strip objcopy addr2line
rm binutils-${BINUTILS_VER}.tar.bz2
