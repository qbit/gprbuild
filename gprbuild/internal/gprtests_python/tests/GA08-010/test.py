from gprbuild_utils import *

gprbuild ("-q --autoconf=default.cgpr", verbose=True, notarget=True)
run ("main")

