import os, string
from gprbuild_utils import *

gprbuild ('prj/prj.gpr')

gprinstall (['-p', '-v', '--prefix='+os.getcwd()+'/inst', 'prj/prj.gpr'],
            output='tmp.out')

gprbuild (['-aPinst/share/gpr', 'main.gpr'])

env.add_dll_path ("inst/bin")

run ("main")
