import os
from gprbuild_utils import *

gprbuild (['-R', 'prj/lib1.gpr'])
gprbuild (['-R', 'prj/lib2.gpr'])

gprinstall (['--prefix='+os.getcwd()+"/inst", 'prj/lib2.gpr'])

if os.environ.get("LIBRARY_PATH") != None:
    old = os.environ["LIBRARY_PATH"]
else:
    old = ""

os.environ["LIBRARY_PATH"] = old + ':' + os.getcwd() + '/prj/lib1'
env.add_dll_path("prj/lib1")
gprbuild (['-aPinst/share/gpr', 'main.gpr'])
env.add_dll_path("inst/lib")
env.add_dll_path("inst/bin")
run("main")
