from gprbuild_utils import *

gprbuild ("-q prj.gpr", verbose=True)
gprbuild ("-q -ws prj.gpr", verbose=True)
cd ("no_sources")
gprbuild ("-q no_c_cpp.gpr", verbose=True)
gprbuild ("-q -ws no_c_cpp.gpr", verbose=True)

