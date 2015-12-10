from gprbuild_utils import *

gprbuild ("prj.gpr");
sleep (4);
touch ("main83.adb");
gprbuild ("prj.gpr", verbose=True);
