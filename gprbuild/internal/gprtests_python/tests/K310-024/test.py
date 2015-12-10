from gprbuild_utils import *
gprbuild("-Pdefault tmp/p.adb")
gprbuild("-Pdefault src/p.adb")
run("p")
