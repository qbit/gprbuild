from gprbuild_utils import *

gprclean ("-q -P ext.gpr");
gprbuild ("-f -q -P orig/orig.gpr");
gprbuild ("-q -P ext.gpr");

