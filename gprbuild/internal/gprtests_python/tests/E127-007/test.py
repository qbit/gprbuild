from gprbuild_utils import *

gprbuild ("-q -Pprj")
run ("bug1")
gprclean ("-q prj.gpr")
