import os
from gprbuild_utils import *

gprbuild (['-q', '-p', 'prj/agg.gpr'])

gprinstall (['-q', '--prefix='+os.getcwd()+"/inst", '-p', 'prj/agg.gpr'])

gprbuild (['-aPinst/share/gpr', 'main.gpr'])

run('main')

if os.path.exists("inst"):
    print "OK, inst directory found"
else:
    print "NOK, no install dir"

if os.path.exists("inst/share/gpr/manifests/azerty"):
    print "OK, manifest/azerty"
else:
    print "NOK, manifest/azerty missing"

if os.path.exists("inst/share/gpr/manifests/querty"):
    print "OK, manifest/querty"
else:
    print "NOK, manifest/querty missing"

if os.path.exists("inst/share/gpr/manifests/agg"):
    print "OK, manifest/agg"
else:
    print "NOK, manifest/agg missing"

gprinstall (['-q', '--prefix='+os.getcwd()+"/inst", '--uninstall', 'agg'])

if os.path.exists("inst"):
    print "NOK, inst directory found"
else:
    print "OK, no install dir"
