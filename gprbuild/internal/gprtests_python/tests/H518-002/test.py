from gprbuild_utils import *

gprbuild ("-f -q prj.gpr", verbose=True)
run ("main")
gprclean ("-q prj.gpr", verbose=True)
gprbuild ("-f -q prj2.gpr", verbose=True)
run ("main")
gprclean ("-q prj2.gpr", verbose=True)
