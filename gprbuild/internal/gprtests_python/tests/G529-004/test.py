from gprbuild_utils import *

gprbuild ("-q -P p1.gpr")
gprbuild ("-P p1.gpr")
ls ("lib/*")
gprclean ("-q -P p1.gpr")
ls ("lib/*")
ls ("obj/*")
