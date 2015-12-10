from gprbuild_utils import *

os.environ ["WHICH"]=which ("gnatmake")
gprbuild ("-f -q prj.gpr", verbose=True)
run ("main")
gprclean ("-q prj.gpr", verbose=True)
