from gprbuild_utils import *

gprbuild ("prj.gpr");
add_dll_dir ("lib3");
run ("ada_main");
