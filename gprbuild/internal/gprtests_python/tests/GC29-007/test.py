from gprbuild_utils import *

cd ("imp")
gprbuild ("-q imp1.gpr -p", verbose=True)
cd ("..")
gprbuild ("-q prj.gpr -p", verbose=True)
