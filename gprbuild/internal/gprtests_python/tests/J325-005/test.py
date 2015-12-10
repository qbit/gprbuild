from gprbuild_utils import *

gprbuild ("prj.gpr");
env.add_dll_path ("p1/lib")
env.add_dll_path ("p2/lib")
env.add_dll_path ("p3/lib")
run ("main");
