import os, stat
from gprbuild_utils import *

# Build

gprbuild (['lib.gpr'])

# Install

gprinstall (['--prefix='+os.getcwd()+"/inst", 'lib.gpr'],
            output='tmp1.out', verbose=True)

# Set api.ali read-only
api=os.path.join(os.getcwd()+"/inst", "lib", "lib", "api.ali")
mode = os.stat(api)[stat.ST_MODE]
os.chmod(api, mode & ~stat.S_IWUSR & ~stat.S_IWGRP & ~stat.S_IWOTH)

gprinstall (['--prefix='+os.getcwd()+"/inst", 'lib.gpr'],
            output='tmp2.out', verbose=True)

print (open("tmp2.out").readlines())

gprinstall (['--prefix='+os.getcwd()+"/inst", '--uninstall', 'lib'],
            output='tmp.out', verbose=True)

if os.path.exists("inst"):
    print "ERROR: inst still there"
else:
    print "OK, removed"
