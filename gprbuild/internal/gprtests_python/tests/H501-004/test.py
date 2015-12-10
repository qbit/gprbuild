from gprbuild_utils import *

gprbuild ("prj.gpr", verbose=True)
gprbuild ("prj.gpr main.2.ada -f", verbose=True)
gprbuild ("prj.gpr toto.c -f", verbose=True)
gprclean ("-q prj.gpr", verbose=True)
