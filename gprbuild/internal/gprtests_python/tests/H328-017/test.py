from gprbuild_utils import *

cp ("a.adb.no_elab", "a.adb")
gprbuild ("-q prj.gpr", verbose=True)
run ("main")
sleep (4)
cp ("a.adb.elab", "a.adb")
touch ("a.adb")
gprbuild ("-q prj.gpr", verbose=True)
run ("main")
