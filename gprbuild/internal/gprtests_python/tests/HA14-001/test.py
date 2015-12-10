from gprbuild_utils import *

cp ("main.adb_orig", "main.adb")
gprbuild ("-q -O2 -gnatws -g -fstack-check -eS prj.gpr", verbose=True)
sleep (4)
cp ("main.adb_new", "main.adb")
touch ("main.adb")
gprbuild ("-m prj.gpr", verbose=True)
gprclean ("-q prj.gpr", verbose=True)
