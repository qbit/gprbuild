from gprbuild_utils import *

gprbuild ("-f -q -p prj.gpr main.adb", verbose=True);
gprclean ("-q prj.gpr main.adb", verbose=True);
