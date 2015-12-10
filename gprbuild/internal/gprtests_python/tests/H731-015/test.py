from gprbuild_utils import *

gprbuild ("-f -q prj.gpr -p", verbose=True)
gprclean ("-q prj.gpr", verbose=True)
