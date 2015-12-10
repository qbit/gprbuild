from gprbuild_utils import *

gprbuild ("-q -p prj.gpr", verbose=True);
run ("main");
gprclean ("-q -r prj.gpr", verbose=True);
gprbuild ("-q -p -XLIB_KIND=relocatable prj.gpr", verbose=True);
