from gprbuild_utils import *

gprbuild ("-Porig/orig")
gprbuild ("-Pext")

# Should not rebind or relink anything
gprbuild ("-Pext", verbose=True)
