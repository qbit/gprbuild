from gprbuild_utils import *

gprname ("-P prj_cross.gpr *.body")
gprbuild ("prj_cross.gpr main.body -o main")
run("main")


