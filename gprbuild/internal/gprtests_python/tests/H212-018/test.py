from gprbuild_utils import *

gprbuild ("-q prj.gpr", verbose=True)
run ("main")
gprclean ("-r -q prj.gpr", verbose=True)
