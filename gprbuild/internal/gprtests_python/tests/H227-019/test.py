from gprbuild_utils import *

gprbuild ("-f -q cpath_fail.gpr", verbose=True)
run ("cpp_main")
gprclean ("-q cpath_fail.gpr", verbose=True)
