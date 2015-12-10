from gprbuild_utils import *

gprbuild ("-XTOTO= -q -P prj.gpr")
run ("toto")
gprclean ("-XTOTO= -q -P prj.gpr")
