from gprbuild_utils import *

gprclean ("-q prj.gpr", verbose=True)
gprbuild ("-q prj.gpr -XLIBRARY_TYPE=static", verbose=True)
gprbuild ("prj.gpr -XLIBRARY_TYPE=static", verbose=True)
gprclean ("-q prj.gpr", verbose=True)
