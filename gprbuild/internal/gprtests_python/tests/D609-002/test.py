from gprbuild_utils import *

gprbuild ("-q -u -P prj main.c")
ls ("objects/main.o*")
gprclean ("-r -q prj.gpr")
