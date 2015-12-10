from gprbuild_utils import *

gprbuild ("-f -q prj.gpr", verbose=True)
Run (["ldd", "main"], output="ldd.txt")
Run (["grep", "libgcc_s", "ldd.txt"], output=None)

