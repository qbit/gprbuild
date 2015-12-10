from gprbuild_utils import *
gprbuild ("-c prj.gpr driver.adb")
ls ("driver.o")
ls ("pkg.o")
