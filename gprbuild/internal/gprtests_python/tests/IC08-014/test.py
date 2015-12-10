from gprbuild_utils import *

Run (["gcc", "-c", "toto.c"], output=None)
gprbuild ("-q -p prj.gpr", verbose=True)

