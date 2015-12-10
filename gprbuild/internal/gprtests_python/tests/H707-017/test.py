from gprbuild_utils import *

gprbuild ("-u prj.gpr main.adb", verbose=True)
gprclean ("-q prj.gpr", verbose=True)
gprbuild ("-c prj.gpr main.adb", verbose=True)
gprbuild ("-u prj.gpr main.adb", verbose=True)
gprbuild ("-u prj.gpr", verbose=True)
gprclean ("-q prj.gpr", verbose=True)
