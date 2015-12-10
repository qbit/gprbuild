from gprbuild_utils import *

gprbuild ("prj2.gpr");
gprbuild ("-q -p prj.gpr", verbose=True)


