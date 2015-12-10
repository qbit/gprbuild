from gprbuild_utils import *

gprbuild ("-q -P maind/prj.gpr")
run ("main")
gprclean ("-q -r maind/prj.gpr")
