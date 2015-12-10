from gprbuild_utils import *

gprbuild ("-f prj.gpr " + os.getcwd() + "\src\main.adb", verbose=True)
gprclean ("-q prj.gpr main.adb", verbose=True)

