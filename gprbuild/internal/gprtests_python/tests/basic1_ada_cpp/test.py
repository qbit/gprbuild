from gprbuild_utils import *

gprbuild()
run("obj/main")
gprclean()
ls("obj/*")
