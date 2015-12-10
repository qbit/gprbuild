import os
from gprbuild_utils import *

# Build

gprbuild (['tools/tools.gpr'])

# Check double install

gprinstall (['--prefix='+os.getcwd()+"/inst", 'lib.gpr'],
            output='tmp1.out', verbose=True)

print (open("tmp1.out").readlines()[0])

gprinstall (['--prefix='+os.getcwd()+"/inst", 'lib.gpr'],
            output='tmp2.out', verbose=True)

print (open("tmp2.out").readlines()[0])

gprinstall (['--prefix='+os.getcwd()+"/inst", '--uninstall', 'lib'],
            output='tmp.out', verbose=True)

# Check install with --install-name

gprinstall (['--prefix='+os.getcwd()+"/inst", '--install-name=myapp',
             'lib.gpr'],
            output='tmp.out', verbose=False)

gprinstall (['--prefix='+os.getcwd()+"/inst", '--install-name=myapp',
             '--mode=usage', 'tools/tools.gpr'],
            output='tmp.out', verbose=False)

m=open("inst/share/gpr/manifests/myapp").readlines()

# remove the sha-1
ms=[]
for l in m[1:]:
    ms = ms + [l[33:]]

# print sorted files to ensure consistent output
for l in sorted(ms):
    print (l)

# Uninstall

gprinstall (['--prefix='+os.getcwd()+"/inst", '--uninstall', 'myapp'],
            output='tmp.out', verbose=True)

if os.path.exists("inst"):
    print "ERROR: inst still there"
else:
    print "OK, removed"
