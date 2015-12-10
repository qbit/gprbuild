from gprbuild_utils import *

gprbuild ("-Ptest")
run ("obj/driver")
gprbuild ("-Ptest -XBUILD=y -f")
run ("obj/driver")
gprclean ("-Ptest -r")
ls ("obj/*")
ls ("subsystem_obj/*")
ls ("subsystem_lib/*")
