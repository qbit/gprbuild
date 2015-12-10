import os
from gprbuild_utils import *

gprinstall (['--prefix='+os.getcwd()+"/inst",
             '--sources-only', '-a', 'prj/lib1.gpr'])

gprbuild (['inst/share/gpr/lib1.gpr'])

gprbuild (['-aPinst/share/gpr', 'main.gpr'])

run('main')

if os.path.exists("inst"):
    print "OK, inst directory found"
else:
    print "NOK, no install dir"

if os.path.exists("inst/lib/lib1/pck1.ali"):
    print "OK, pck1.ali found"
else:
    print "NOK, pck1.ali missing"
