from gprbuild_utils import *

cd ("c")
gprbuild ("toto.gpr")
cd ("..")
gprbuild ("-f -q -p prj.gpr", verbose=True)
run ("main")
gprbuild ("-f -q prj2.gpr", verbose=True)
run ("main2")
