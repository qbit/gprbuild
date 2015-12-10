from gprbuild_utils import *

os.environ["LIB"] = ""

# now test subsystem as a static library
gprbuild ("-Pc_main -XLIB=static -cargs:c -w")
run ("c_main")
# rebuild forcing incorrect compiler option for the subsystem
sleep (4)
gprbuild (" -f -Psubsystem -XLIB=static -cargs:c -DXXX")
# rebuilding the main should happen
gprbuild ("c_main.gpr")
# and the executable should report an error about XXX defined
run ("c_main")
gprclean ("-r -XLIB=static -Pc_main")
#check that all object dirs are empty
ls ("c_obj/*")
ls ("subsystem_obj/*")
ls ("slib/*")
