from gprbuild_utils import *

gprbuild ("ft1.gpr -cargs:ada -gnatws")
run ("fobj1/ess")
gprclean ("-r ft1.gpr")
ls ("fobj1/*")
ls ("obj/*")
ls ("lib/*")
