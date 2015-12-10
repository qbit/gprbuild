from gprbuild_utils import *

gprbuild ("-q -ws prj2.gpr", verbose=True)
gprbuild ("-q prj3.gpr", verbose=True)
