from gprbuild_utils import *

gprbuild ("-q prj.gpr", verbose=True);
gprclean ("-q prj.gpr", verbose=True);
