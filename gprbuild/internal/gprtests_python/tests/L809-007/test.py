from gprbuild_utils import *

gprbuild ("prj.gpr")
ls ("copies/*.h")
ls ("copies/*.ads");
gprclean ("prj.gpr")
ls ("obj/*")

