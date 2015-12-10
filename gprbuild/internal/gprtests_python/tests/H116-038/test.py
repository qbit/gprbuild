from gprbuild_utils import *

gprbuild ("gener.gpr")
run ("gener")
gprbuild ("-q prj.gpr")

