from gprbuild_utils import *

gprclean ("-q -r prj.gpr", verbose=True)
gprbuild ("-q prj.gpr", verbose=True)
sleep (4)
cd ("imp")
touch ("pkg.adb")
cd ("..")
gprbuild ("prj.gpr", verbose=True)
gprbuild ("prj.gpr", verbose=True)
gprclean ("-r -q prj.gpr", verbose=True)
