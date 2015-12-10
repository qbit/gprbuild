from gprbuild_utils import *

gprbuild ("-q -P prj")
env.add_dll_path ("imp/lib/")
run ("main_c")
gprclean ("-q -r prj.gpr")
