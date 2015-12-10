import os
from gprbuild_utils import *

# Build

gprbuild (['lib.gpr'])

# Check double install

gprinstall (['--prefix='+os.getcwd()+"/inst", '--build-name=1', 'lib.gpr'],
            output='tmp1.out', verbose=True)

gprinstall (['--prefix='+os.getcwd()+"/inst", '--build-name=2', 'lib.gpr'],
            output='tmp2.out', verbose=True)

len = 0

for line in open('inst/share/gpr/lib.gpr'):
    if line[0:30] == "            for Linker_Options":
        if line[-3:-1] != ");":
            print("found new-line in attribute!");
