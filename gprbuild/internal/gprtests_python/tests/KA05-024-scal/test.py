from gprbuild_utils import *

gprbuild ("-q -p -XLIBRARY_TYPE=relocatable shared/shared.gpr", verbose=True)
gprclean ("-q -XLIBRARY_TYPE=relocatable shared/shared.gpr", verbose=True)
gprbuild ("-q -p -XLIBRARY_TYPE=static shared/shared.gpr", verbose=True)
gprclean ("-q -XLIBRARY_TYPE=static shared/shared.gpr", verbose=True)
gprbuild ("-q -p -XLIBRARY_TYPE=static prj.gpr", verbose=True)
env.add_dll_path ("shared/lib")
run("main");
gprclean ("-q -r -XLIBRARY_TYPE=static prj.gpr")
