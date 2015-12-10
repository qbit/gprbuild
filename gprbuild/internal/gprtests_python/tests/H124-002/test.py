from gprbuild_utils import *

cd ("ois")
gprbuild ("-q ois_build.gpr", verbose=True)
cd ("..")
gprbuild ("-q prj.gpr", verbose=True)
run ("main")
