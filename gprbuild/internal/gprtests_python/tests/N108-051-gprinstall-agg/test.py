import os
from gprbuild_utils import *

# For default

gprbuild (['prj.gpr'])
gprbuild (['lib.gpr'])

gprinstall (['--prefix='+os.getcwd()+"/inst1",' prj.gpr'],
            output='tmp.out', verbose=True)
gprinstall (['--prefix='+os.getcwd()+"/inst2",' lib.gpr'],
            output='tmp.out', verbose=True)
