from gprbuild_utils import *

gprbuild ("-q prj.gpr", verbose=True)
gprbuild ("-q prj2.gpr", verbose=True)
gprbuild ("-q prj3.gpr", verbose=True)
gprbuild ("-q prj4.gpr", verbose=True)
gprclean ("-q prj2.gpr")
