from gprbuild_utils import *

gprbuild ("aggr.gpr")
gprclean ("aggr.gpr")

gprbuild ("agglib.gpr", verbose=True)
gprclean ("agglib.gpr")

