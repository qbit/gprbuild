from gprbuild_utils import *

gprbuild ("prj.gpr -f -q", verbose=True)
gprclean ("prj.gpr -q", verbose=True)
