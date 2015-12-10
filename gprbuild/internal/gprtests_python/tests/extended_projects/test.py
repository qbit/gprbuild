from gprbuild_utils import *

gprbuild ("-Pnew_ada_main.gpr")
run ("ada_main")
gprclean ("-r -Pnew_ada_main.gpr")
ls ("new_obj/*")
ls ("new_obj_util/*")
ls ("subsystems/obj/*")
ls ("subsystems/obj_util/*")
