from gprbuild_utils import *

gprbuild ("-q -P prj")
run ("main")
gprclean ("-q -r prj.gpr")
