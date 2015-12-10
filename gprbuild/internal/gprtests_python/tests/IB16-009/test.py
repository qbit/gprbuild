from gprbuild_utils import *

gprbuild ("prj.gpr", verbose=True)
gprclean ("-r -q prj.gpr", verbose=True)
