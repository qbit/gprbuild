from gprbuild_utils import *

cd ("toto")
gprbuild ("-q toto.gpr", verbose=True)
cd ("..")
gprbuild ("-q prj.gpr ", verbose=True)
gprclean ("-q prj.gpr", verbose=True)
