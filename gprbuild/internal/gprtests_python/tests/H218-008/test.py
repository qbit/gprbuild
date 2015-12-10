from gprbuild_utils import *

gprbuild ("-q -f prj.gpr", verbose=True)
run ("main")
gprclean ("-q prj.gpr", verbose=True)
