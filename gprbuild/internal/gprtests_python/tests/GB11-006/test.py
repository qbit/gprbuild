from gprbuild_utils import *

gprbuild ("-q static_lib.gpr", verbose=True)
cd ("lib")
ls ("*.a")
cd ("..")
gprclean ("-q static_lib.gpr", verbose=True)
