import os
from gprbuild_utils import *

# For default

gprbuild (['-XOS=UNIX', 'prj/prj.gpr'])
gprinstall (['--prefix='+os.getcwd()+"/inst", '-XOS=UNIX', 'prj/prj.gpr'],
            output='tmp.out', verbose=False)

# For NT

gprbuild (['-XOS=Windows_NT', 'prj/prj.gpr'])
gprinstall (['--prefix='+os.getcwd()+"/inst", '-XOS=Windows_NT',
             '--build-name=nt', 'prj/prj.gpr'],
            output='tmp.out', verbose=False)

# Test default

gprbuild (['-aPinst/share/gpr', 'main.gpr'])
run ("main")

# Test NT

res = Run (['gprclean', '-q', '-aPinst/share/gpr', 'main.gpr'])
gprbuild (['-aPinst/share/gpr', '-XPRJ_BUILD=nt', 'main.gpr'])
run ("main")

# Uninstall

gprinstall (['--prefix='+os.getcwd()+"/inst", '--uninstall', 'prj.gpr'],
            output='tmp.out', verbose=True)

if os.path.exists("inst"):
    print "ERROR: inst still there"
else:
    print "OK, removed"
