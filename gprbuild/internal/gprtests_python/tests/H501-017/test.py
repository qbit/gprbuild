from gprbuild_utils import *

cd ("base")
gprbuild ("-q -P base.gpr", verbose=True)
cd ("..")
gprbuild ("-q -P prj.gpr", verbose=True)
