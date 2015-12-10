from gprbuild_utils import *

gprbuild ("prj.gpr")
run("main")
gprbuild ("prj.gpr", verbose=True)

