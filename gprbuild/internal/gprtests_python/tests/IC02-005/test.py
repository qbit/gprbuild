from gprbuild_utils import *

gprbuild ("-q prj.gpr", verbose=True)
gprbuild ("-f -q -x prj.gpr", verbose=True)
run ("main")
