from gprbuild_utils import *

gprbuild ("main.adb");
run ("main")
gprclean ("main.adb");
