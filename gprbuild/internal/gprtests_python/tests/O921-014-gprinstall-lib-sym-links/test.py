import os, string, platform
from gprbuild_utils import *
from sys import platform as _platform

gprbuild ('prj/prj.gpr')

gprinstall (['-p', '-v', '--prefix='+os.getcwd()+'/inst', 'prj/prj.gpr'],
            output='tmp.out')

gprbuild (['-aPinst/share/gpr', 'main.gpr'])

env.add_dll_path ("inst/bin")

# only check for library version on non Windows OS

files = []

if _platform == "linux" or _platform == "linux2":
    files = [ "inst/lib/libprj.so",
              "inst/lib/libprj.so.1",
              "inst/lib/libprj.so.1.2",
              "prj/lib/libprj.so",
              "prj/lib/libprj.so.1",
              "prj/lib/libprj.so.1.2" ]
elif _platform == "darwin":
    files = [ "inst/lib/libprj.dylib",
              "inst/lib/libprj.so.1.2",
              "prj/lib/libprj.dylib",
              "prj/lib/libprj.so.1.2" ]
# note that above the .so is because this is what is in the project file.
# not a real issue.

for f in files:
    if not os.path.exists(f):
        print("missing: " + f)

run ("main")
