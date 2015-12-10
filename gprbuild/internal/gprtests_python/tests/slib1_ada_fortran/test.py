from gprbuild_utils import *

gprbuild ("-P ft2.gpr -cargs:ada -gnatws")
run ("fobj2/ess2")
gprclean ("-P ft2.gpr -r")
ls ("fobj2/*")
ls ("slib/*")
ls ("sobj/*")
