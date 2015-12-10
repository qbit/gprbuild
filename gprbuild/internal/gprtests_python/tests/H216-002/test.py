from gprbuild_utils import *

gprbuild ("-q root.gpr", verbose=True)
run ("a")
gprclean ("-q root.gpr", verbose=True)
