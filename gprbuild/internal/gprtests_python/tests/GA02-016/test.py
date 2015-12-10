from gprbuild_utils import *

gprbuild ("-q")
run ("obj/hello")

# no constraint_error should be raised
gprclean ("-q dummy_source.adb")
