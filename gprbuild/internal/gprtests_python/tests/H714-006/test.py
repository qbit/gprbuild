from gprbuild_utils import *

gprbuild ("-q prj.gpr", verbose=True)
gprclean ("-r -q prj.gpr", verbose=True)
