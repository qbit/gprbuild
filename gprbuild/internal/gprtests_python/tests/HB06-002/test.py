from gprbuild_utils import *

cd ("lib2")
Run (["gcc", "-c", "toto.c"], output=None)
Run (["ar", "rc", "libtoto.a", "toto.o"], output=None)
cd ("..")
gprbuild ("-q -p prj.gpr", verbose=True)
