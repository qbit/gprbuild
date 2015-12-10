from gprbuild_utils import *

gprname ("-P prj.gpr *.ada")
gprbuild ("prj.gpr main.2.ada -o main")
run("main")


