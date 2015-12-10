from gprbuild_utils import *

gprbuild ("prj.gpr -q", verbose=True)
run ("main")
gprclean ("prj.gpr -q", verbose=True)
gprbuild ("prj.gpr -XBUILD=CHECK -c", verbose=True)

