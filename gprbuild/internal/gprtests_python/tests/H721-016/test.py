from gprbuild_utils import *

cd ("extern")
gprbuild ("pkg.gpr")
cd ("..")
os.chmod ("extern", 0555)
gprbuild ("-s -q prj.gpr", verbose=True)
run ("main")
os.chmod ("extern", 0777)
