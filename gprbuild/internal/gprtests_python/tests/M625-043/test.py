from gprbuild_utils import *

gprbuild ("prj.gpr")
sleep (3.5);
touch ("main.adb")
gprbuild ("prj.gpr")
run ("main")
