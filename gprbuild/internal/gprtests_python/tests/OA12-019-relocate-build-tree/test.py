import os, stat
from gprbuild_utils import *

# Build

gprbuild (['--relocate-build-tree=' + os.getcwd()+'/build-dir',
           '--root-dir=' + os.getcwd(), 'path/to/prj/p.gpr'])

if os.path.exists("build-dir"):
    print "OK, build-dir"
else:
    print "NOK, build-dir not found"

if os.path.exists("build-dir/path/to/prj/obj/main.ali"):
    print "OK, main.ali"
else:
    print "NOK, main.ali not found"
