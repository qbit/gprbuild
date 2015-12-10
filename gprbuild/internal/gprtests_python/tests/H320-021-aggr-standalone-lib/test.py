from gprbuild_utils import *
import os, sys

os.mkdir ("o1lib")
os.mkdir ("o2lib")
os.mkdir ("a1lib")
os.mkdir ("a2lib")
os.mkdir ("laggr")

gprbuild("main.gpr")

if os.path.isfile("b__main.o"):
    print "Found file b__main.o"

add_dll_dir ("laggr")
run("main");
