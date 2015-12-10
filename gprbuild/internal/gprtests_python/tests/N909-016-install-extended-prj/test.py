import os
from gprbuild_utils import *

# Build

gprbuild (['-p', '-q', 'lib.gpr'])

# Check double install

gprinstall (['-p', '-v', '-m', '--prefix='+os.getcwd()+"/inst1", 'lib.gpr'],
            output='tmp1.out', verbose=True)
gprinstall (['-p', '-v', '--prefix='+os.getcwd()+"/inst2", 'lib.gpr'],
            output='tmp2.out', verbose=True)

for l in sorted(open("tmp1.out").readlines()):
    if l[0:3] in ("Ins", "cp ", "Pro"):
        print l

for l in sorted(open("tmp2.out").readlines()):
    if l[0:3] in ("Ins", "cp ", "Pro"):
        print l
