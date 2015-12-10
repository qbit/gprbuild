#!/bin/sh
rm -f *.o
gcc  -c -Wall -D_GNU_SOURCE cpu_affinity.c
ar cr ../builds/x86_64-pc-linux-gnu/lib/libcpu_affinity.a cpu_affinity.o
