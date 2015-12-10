from gprbuild_utils import *

gprbuild ("main.gpr")
run ("main")
gprclean ("-r main.gpr")

