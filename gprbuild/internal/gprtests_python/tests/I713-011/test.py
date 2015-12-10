from gprbuild_utils import *

gprbuild ("-Pdefault.gpr")
gprclean ("-Pdefault.gpr")
ls ("*")
