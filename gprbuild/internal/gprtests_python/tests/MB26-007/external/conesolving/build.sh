#!/bin/sh

LIBDIR=../builds/x86_64-pc-linux-gnu/lib
export DSDPROOT=`pwd`/DSDP5.8
cd DSDP5.8
make -j4
cd ..

cp $DSDPROOT/lib/libdsdp.a $LIBDIR
