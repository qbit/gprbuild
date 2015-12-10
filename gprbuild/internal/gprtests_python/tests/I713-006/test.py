from gprbuild_utils import *

cd ("prj")
gprbuild ("-p -q prj.gpr", verbose=True)
cd ("..")
run ("prj/main")
gprclean ("-q -r prj/prj.gpr", verbose=True)
