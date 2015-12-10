from gprbuild_utils import *

cd ("ois")
gprbuild ("ois_build.gpr")
cd ("..")
gprbuild ("prj.gpr")
run ("main")
