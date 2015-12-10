from gprbuild_utils import *

gprbuild ("-q -P prj")
run ("main")
gprclean ("-r -q prj.gpr")
