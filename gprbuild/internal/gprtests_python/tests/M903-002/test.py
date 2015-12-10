from gprbuild_utils import *

gprbuild ("-c -q prj.gpr", output=None)
gprbuild ("-b -q prj.gpr", output=None)
gprbuild ("-l -q prj.gpr", output=None)
