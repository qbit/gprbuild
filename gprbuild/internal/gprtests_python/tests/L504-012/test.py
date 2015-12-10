from gprbuild_utils import *

gprbuild ("prj.gpr")
gprbuild ("prj.gpr --autoconf=auto.cgpr", verbose=True)

