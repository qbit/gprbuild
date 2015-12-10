from gprbuild_utils import *

gprbuild (["-p", "-q", "-Psrc.gpr"])
gprbuild ("-Psrc.gpr", verbose=True)
