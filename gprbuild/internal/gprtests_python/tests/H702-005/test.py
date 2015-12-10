from gprbuild_utils import *

gprbuild ("-q main.gpr -p", verbose=True)
gprclean ("-q main.gpr", verbose=True)
