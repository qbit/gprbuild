#!/bin/sh
cd `dirname $0`
rm -f *.o
gcc  -c -Wall -D_GNU_SOURCE valgrind.c 
ar cr ../builds/x86_64-pc-linux-gnu/lib/libvalgrind.a valgrind.o
