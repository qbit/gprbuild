from gprbuild_utils import *

gprbuild ("prji.gpr");
gprbuild ("prji.gpr", verbose=True);
gprclean ("-r prji.gpr");
