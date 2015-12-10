from gprbuild_utils import *

gprbuild ("-q new_ada_main.gpr")
run ("ada_main")
gprclean ("-r -q new_ada_main.gpr")
