from gprbuild_utils import *

gprbuild ("-q -p prj-ch1-ch2.gpr", verbose=True)
run ("main")
gprclean ("-r -q prj-ch1-ch2.gpr", verbose=True)
