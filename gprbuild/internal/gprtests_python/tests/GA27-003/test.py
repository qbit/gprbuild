from gprbuild_utils import *

gprbuild ("-q -p static_lib.gpr", verbose=True)
gprbuild ("-q -p main5.gpr", verbose=True)
run ("main5")
