from gprbuild_utils import *

cd ("base")
gprbuild ("-q base.gpr", verbose=True)
cd ("..")
gprbuild ("-q prj.gpr", verbose=True)
