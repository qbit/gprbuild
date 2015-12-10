from gprbuild_utils import *

env.add_dll_path ("shared/lib")
gprbuild ("-q -P prj")
run ("main")
gprclean ("-q -r prj.gpr")
