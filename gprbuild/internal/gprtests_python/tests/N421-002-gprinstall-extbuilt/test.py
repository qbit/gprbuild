import os, string
from gprbuild_utils import *

gprbuild ('lib/lib1.gpr')
gprbuild ('lib/lib2.gpr')

gprinstall (['-p', '--prefix='+os.getcwd()+'/inst', 'lib/lib1e.gpr'],
            output='tmp.out')
gprinstall (['-p', '--prefix='+os.getcwd()+'/inst', 'lib/lib2e.gpr'],
            output='tmp.out')

gprbuild (['-aPinst/share/gpr', 'main.gpr'])

run ("main")
