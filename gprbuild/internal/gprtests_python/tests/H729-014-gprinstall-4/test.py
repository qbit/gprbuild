import os
from gprbuild_utils import *

def print_inst(lines):
    print lines[0]

    # cp or ln lines to be sorted
    cp=[]
    # remaining lines
    r=[]
    for l in lines[1:]:
        if l[0:3] == 'cp ' or l[0:3] == 'ln ':
            cp = cp + [l]
        else:
            r = r + [l]

    cp.sort()
    for l in cp:
        print l
    for l in r:
        print l

# For default

gprbuild (['-XOS=UNIX', 'prj/prj.gpr'])
gprinstall (['-r', '--prefix='+os.getcwd()+"/inst",
             '-XOS=UNIX', 'prj/prj.gpr'],
            output='tmp.out', verbose=True)

# Test default

gprbuild (['-aPinst/share/gpr', 'main.gpr'])
run ("bin/mymain")

# Install

gprinstall (['--dry-run', '--prefix='+os.getcwd()+"/inst",
             '-aPinst/share/gpr', '--mode=usage', 'main.gpr'],
            output='tmp.out', verbose=True)
print_inst (open('tmp.out').readlines())

# Uninstall

gprinstall (['--prefix='+os.getcwd()+"/inst", '--uninstall', 'prj'],
            output='tmp.out', verbose=True)

if os.path.exists("inst"):
    print "ERROR: inst still there"
else:
    print "OK, removed"
