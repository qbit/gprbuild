from gprbuild_utils import *

gprbuild ("-P prj.gpr -XBUILD=y")
run ("main")
ls ("obj/*")
print "---"
gprclean ("-P prj.gpr -XBUILD=y")
ls ("obj/*")
