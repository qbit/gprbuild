from gprbuild_utils import *
import time

# now test subsystem as a dynamic library
env.add_dll_path ("dlib")
gprbuild ("-Pc_main -XLIB=dynamic -cargs:c -w")
run ("c_main")
# rebuild forcing incorrect compiler option for the subsystem
time.sleep (2)
gprbuild (" -f -Psubsystem -XLIB=dynamic -cargs:c -DXXX")
# rebuilding the main should happen
gprbuild (" c_main.gpr -XLIB=dynamic")
# and the executable should report an error about XXX defined
run ("c_main")
gprclean ("-r -XLIB=dynamic -Pc_main")
#check that all object dirs are empty
ls ("c_obj/*")
ls ("subsystem_obj/*")
ls ("dlib/*")
