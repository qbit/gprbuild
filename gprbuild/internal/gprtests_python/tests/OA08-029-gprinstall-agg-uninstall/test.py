import os
from gprbuild_utils import *

# For default

gprbuild (['agg.gpr'])
gprinstall (['--prefix='+os.getcwd()+"/inst", 'agg.gpr'],
            output='tmp.out', verbose=True)

gprinstall (['--uninstall', '--prefix='+os.getcwd()+"/inst", 'agg.gpr'],
            output='tmp.out', verbose=True)

if os.path.exists("inst"):
    print("aggregate project not fully uninstalled")
else:
    print("OK")
