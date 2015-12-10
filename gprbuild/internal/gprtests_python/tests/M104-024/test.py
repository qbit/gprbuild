from gprbuild_utils import *

cd ("lib")
gprbuild ("lib_build.gpr");
cd ("..")
gprbuild ("prj.gpr");
run ("main")
sleep (3.5)
cd ("lib")
cp ("toto.c_new", "toto.c")
touch ("toto.c")
gprbuild ("lib_build.gpr");
cd ("..")
gprbuild ("prj.gpr");
run ("main")
