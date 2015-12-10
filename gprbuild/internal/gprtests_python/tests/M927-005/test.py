from gprbuild_utils import *

gprbuild ("prj.gpr toto.c")
gprbuild ("prj.gpr prj1/toto.c")
run ("prj1/toto")
gprbuild ("prj.gpr prj2/toto.c")
run ("prj2/toto")
gprbuild ("prj.gpr prj3/toto.c")
