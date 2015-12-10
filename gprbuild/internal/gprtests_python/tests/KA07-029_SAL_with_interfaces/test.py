from gprbuild_utils import *

env.add_dll_path ("lib");
gprbuild ("prj.gpr");
run ("main");
