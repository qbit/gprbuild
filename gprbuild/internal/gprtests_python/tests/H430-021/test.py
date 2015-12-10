from gprbuild_utils import *

gprclean ("-q prj.gpr")
gprbuild ("-f -c main.gpr")
gprbuild ("prj.gpr", verbose=True)
gprbuild ("prj.gpr", verbose=True)
run ("main")
gprbuild ("-f -c main.gpr")
gprbuild ("-s prj.gpr", verbose=True)
gprbuild ("-s prj.gpr", verbose=True)
run ("main")
gprbuild ("-f -c main.gpr")
sleep (8)
gprbuild ("prj.gpr", verbose=True)


