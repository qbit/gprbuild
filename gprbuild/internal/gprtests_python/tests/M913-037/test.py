from gprbuild_utils import *

gprbuild ("-c aggr.gpr", verbose=True, output="output.txt")
Run (["sort", "output.txt"], output=None)

