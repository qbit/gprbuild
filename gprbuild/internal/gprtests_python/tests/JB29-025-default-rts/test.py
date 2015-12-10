from gprbuild_utils import *

# first case
gprbuild(["--target=" + env.target.triplet, "-Pbasic1"])
run("main")
