from gprbuild_utils import *

gprbuild("-Ptest")
run("obj/backtraces", output="test.res")
count("backtraces.adb:9", "test.res")
count("backtraces.adb:12", "test.res")
count("backtraces.adb:15", "test.res")
gprclean("-Ptest")
ls("obj/*")
