from gprbuild_utils import *

gprname ("-P prj.gpr *.ada -f *.c -f:c *.clang")
gprbuild ("prj.gpr main.ada")
run ("main")

