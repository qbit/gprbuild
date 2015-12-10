from gprbuild_utils import *

gprbuild ("-q -P prj.gpr")
run ("main")
gprclean ("-r -q prj.gpr")
