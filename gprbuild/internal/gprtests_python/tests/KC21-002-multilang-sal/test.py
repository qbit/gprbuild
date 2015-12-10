from gprbuild_utils import *

gprbuild ("prj.gpr");
env.add_dll_path ("sal/lib")
run ("main");
