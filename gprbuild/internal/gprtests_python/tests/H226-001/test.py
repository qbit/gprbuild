from gprbuild_utils import *

gprbuild ("-q -p -ws prj.gpr", verbose=True)
run ("main")
