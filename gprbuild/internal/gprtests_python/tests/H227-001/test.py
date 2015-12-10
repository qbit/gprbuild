from gprbuild_utils import *

gprbuild ("-q -p err.gpr", verbose=True)
run ("err")
gprclean ("-q err.gpr", verbose=True)
