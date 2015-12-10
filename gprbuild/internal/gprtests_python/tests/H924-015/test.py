from gprbuild_utils import *

gprbuild ("toto.gpr")
gprbuild ("-q prj.gpr", verbose=True)
run ("main")
