from gprbuild_utils import *
import os, sys

gprbuild("-q -p extlib/win32ada.gpr", verbose=False)
gprbuild("-p main.gpr -largs -v -v", verbose=True, output="bmain.out")
add_dll_dir ("lib")
run("main");

# now check that -lwin32ada was not on the main link line

f = open('bmain.out')
for line in f:
    if '-o main' in line and '-lwin32ada' in line:
        print 'ERROR: -lwin32ada found in main link command line'
        print line
