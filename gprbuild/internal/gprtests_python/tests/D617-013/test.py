from gprbuild_utils import *

gprbuild ("-q -P prj")
run ("obj/main")
gprclean ("-q -r prj.gpr")
