import os
from gprbuild_utils import *

res = Run (['gprbuild', '-q', '-p', 'agg.gpr'])

res = Run (['gprinstall', '-p', '--prefix='+os.getcwd()+"/inst", 'agg.gpr'])

res = Run (['gprbuild', '-q', '-aPinst/share/gpr', 'main.gpr'])

run ("main")

res = Run (['gprinstall', '--prefix='+os.getcwd()+"/inst",
            '--uninstall', 'lib1.gpr'])
res = Run (['gprinstall', '--prefix='+os.getcwd()+"/inst",
            '--uninstall', 'lib2.gpr'])

if os.path.exists("inst") and os.path.exists("inst/share/gpr/manifests/agg"):
    print "OK, agg manifest still there"
else:
    print "NOK, removed"

res = Run (['gprinstall', '--prefix='+os.getcwd()+"/inst",
            '--uninstall', 'agg.gpr'])

if os.path.exists("inst"):
    print "ERROR: inst still there"
else:
    print "OK, removed"
