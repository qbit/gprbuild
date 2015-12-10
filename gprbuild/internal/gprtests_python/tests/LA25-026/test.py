from gprbuild_utils import *

gprbuild ("-q -p aggr2.gpr", verbose=True, notarget=True)
gprbuild ("-q aggr3.gpr", verbose=True, notarget=True)
gprbuild ("-q aggr4.gpr", verbose=True, notarget=True)
gprbuild ("-q aggr5.gpr", verbose=True, notarget=True)
