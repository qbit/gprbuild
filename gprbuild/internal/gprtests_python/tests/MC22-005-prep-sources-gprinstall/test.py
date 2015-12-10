import os
from gprbuild_utils import *

# For default

gprbuild (['-p', '-gnateDTARGET=Win32', '-gnateG', 'lib/lib.gpr'])
gprinstall (['-q', '-p', '-f', '--prefix='+os.getcwd()+"/inst", 'lib/lib.gpr'],
            output='tmp.out', verbose=True)

gprbuild (['-p', '-f', '-gnateDTARGET=Win64', '-gnateG', 'lib/lib.gpr'])
gprinstall (['-q', '-p', '-f', '--prefix='+os.getcwd()+"/inst",
             '--build-name=Win64', 'lib/lib.gpr'],
            output='tmp.out', verbose=True)

# Test it

gprbuild (['-p', '-aPinst/share/gpr', 'prj.gpr'])

run("main");

gprbuild (['-p', '-f', '-XLIB_BUILD=Win64', '-aPinst/share/gpr', 'prj.gpr'])

run("main");
