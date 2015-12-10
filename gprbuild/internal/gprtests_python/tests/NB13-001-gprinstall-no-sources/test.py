import os
from gprbuild_utils import *

gprinstall (['--prefix='+os.getcwd()+"/inst", 'prj.gpr'])

if os.path.exists("inst"):
    print "OK, inst directory found"
else:
    print "NOK, no install dir"

if os.path.exists("inst/share/gpr/prj.gpr"):
    print "OK, prj.gpr found"
else:
    print "NOK, prj.gpr missing"
