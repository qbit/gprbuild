from gprbuild_utils import *

gprbuild ("-f -q ext.gpr", verbose=True)
run ("proof")
gprclean ("-q -r ext.gpr", verbose=True)
