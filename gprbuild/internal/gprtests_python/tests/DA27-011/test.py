from gprbuild_utils import *

gprbuild("-c -q -P prj")
ls("toto.o*")
gprclean("-r -q prj.gpr")
