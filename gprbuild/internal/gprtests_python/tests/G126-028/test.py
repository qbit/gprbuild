from gprbuild_utils import *

gprbuild ("-q -P main")
run ("main_cpp")
gprclean ("-r main.gpr")
ls ("obj/*")
