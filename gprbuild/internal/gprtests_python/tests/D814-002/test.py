from gprbuild_utils import *

gprbuild ("-q -P compile_c")
run ("hello")
gprclean ("-q -r compile_c.gpr")
