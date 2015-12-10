from gprbuild_utils import *

gprbuild ("-q -P build.gpr")
run ("main")
gprclean ("-q build.gpr")
