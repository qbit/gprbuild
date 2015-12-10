from gprbuild_utils import *

gprbuild ("make_sources.gpr")
run ("make_sources")
gprbuild ("-j0 -q prj.gpr", verbose=True)

