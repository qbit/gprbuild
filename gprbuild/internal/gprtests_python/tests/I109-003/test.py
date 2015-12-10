from gprbuild_utils import *

gprbuild ("-f -q -p prj.gpr", verbose=True)
run ("main")
