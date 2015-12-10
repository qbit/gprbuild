from gprbuild_utils import *

gprbuild ("-f -q -p prj.gpr")
sleep (4)
touch ("pkg.adb")
gprbuild ("-q prj.gpr")
gprbuild ("prj.gpr", verbose=True);
