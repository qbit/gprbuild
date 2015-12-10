from gprbuild_utils import *

gprbuild ("-q -P prj")
gprbuild ("-P prj", verbose=True)
run ("hello_ada")
