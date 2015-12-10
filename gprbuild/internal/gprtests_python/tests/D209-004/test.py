from gprbuild_utils import *

gprbuild ("-q -P test")
run ("objects/test")
gprclean ("-q -r -P test.gpr")
