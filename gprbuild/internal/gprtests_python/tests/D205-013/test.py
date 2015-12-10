from gprbuild_utils import *

cd ("imp")
cd ("c_code")
gprbuild ("titi.gpr")
cd ("..")
cd ("..")
cd ("imp2")
cd ("c_code")
gprbuild ("tata.gpr -c tata.c")
cd ("..")
cd ("..")
gprbuild ("-q -P prj", verbose=True)
run ("main")
