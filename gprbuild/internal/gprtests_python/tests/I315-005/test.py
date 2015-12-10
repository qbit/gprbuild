from gprbuild_utils import *

gprclean ("-q prj.gpr", verbose=True)
gprbuild ("-q prj.gpr -cargs -gnatws", verbose=True)
gprbuild ("prj.gpr -cargs -gnatws", verbose=True)
gprclean ("-q prj.gpr", verbose=True)
