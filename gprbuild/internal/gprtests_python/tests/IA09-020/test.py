from gprbuild_utils import *

gprbuild ("prj.gpr")
add_dll_dir ("lib1/lib")
add_dll_dir ("lib2/lib")
add_dll_dir ("lib3/lib")
run("main")

