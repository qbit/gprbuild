from gprbuild_utils import *
import os, sys

def display_clean():
    for file in sorted(open("clean.out").readlines()):
        print file

os.mkdir ("o1lib")
os.mkdir ("o2lib")
os.mkdir ("a2lib")
os.mkdir ("laggr")

print "=================== RUN1"
gprbuild("aggr.gpr", verbose=True)
gprclean("-r aggr.gpr", verbose=True, output="clean.out")
display_clean()

print "=================== RUN3"
gprbuild("main.gpr", verbose=True)
run("main")
gprclean("-r main.gpr", verbose=True, output="clean.out")
display_clean()
