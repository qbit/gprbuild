from gprbuild_utils import *

# This test makes sure that when using -k, an error in one of the aggregated
# projects does not prevent the building and linking of the others.

gprbuild(["-k", "-p", "aggr.gpr"], verbose=True)
