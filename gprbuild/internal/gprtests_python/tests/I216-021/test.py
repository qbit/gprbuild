from gprbuild_utils import *

gprbuild ("-f -q prj1.gpr main1.adb")
gprbuild ("-f -q prj1.gpr main2.adb")
gprbuild ("-f -q prj2.gpr main1.adb")
gprbuild ("-f -q prj2.gpr main2.adb")
