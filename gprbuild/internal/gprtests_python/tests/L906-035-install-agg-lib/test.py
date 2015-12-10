import os
from gprbuild_utils import *

res = Run (['gprbuild', '-q', '-p', 'agg.gpr'])

res = Run (['gprinstall', '-p', '--prefix='+os.getcwd()+"/inst", 'agg.gpr'])

res = Run (['gprbuild', '-q', '-aPinst/share/gpr', 'main.gpr'])

run ("main")

res = Run (['gprinstall', '--prefix='+os.getcwd()+"/inst",
            '--uninstall', 'agg.gpr'])

if os.path.exists("inst"):
    print "ERROR: inst still there"
else:
    print "OK, removed"
