from gprbuild_utils import *

os.mkdir ("dir with spaces")
mv ("toto.c", "dir with spaces")
gprbuild ("-q -P prj.gpr")
gprbuild ("-P prj.gpr -c")
run ("main")
gprclean ("prj.gpr")
