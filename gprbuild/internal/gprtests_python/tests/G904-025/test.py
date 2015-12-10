from gprbuild_utils import *

print ("no main")
gprbuild ("-q prj.gpr")
ls ("lib*")

print ("with a main")
gprbuild ("-q prj.gpr main.adb")
ls ("libprj.a")
ls ("libprj.deps")
run ("main")
