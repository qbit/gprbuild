from gprbuild_utils import *

# This test makes sure that we only link the executables once.

gprbuild("aggr.gpr", verbose=True)
