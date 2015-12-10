from gprbuild_utils import *

gprbuild ("-q -p simple.gpr", verbose=True)
cd ("obj")
rm ("chop.ali")
cd ("..")
sleep (3)
gprbuild ("-q -c simple.gpr chop.adb -cargs -gnatc", verbose=True)
gprbuild ("-q simple.gpr", verbose=True)
