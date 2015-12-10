from gprbuild_utils import *

gprbuild(["-f", "-P", "prj"])
run("main")
gprclean(["-r", "prj.gpr"])
