import os, string
from gprbuild_utils import *

gprbuild ('prj/lib.gpr')

gprinstall (['-p', '-v', '--prefix='+os.getcwd()+'/inst', 'prj/lib.gpr'],
            output='tmp.out')

gprbuild (['-aPinst/share/gpr', 'main.gpr'])

run ("main")
