from gprbuild_utils import *

# now test subsystem as a static library
add_dll_dir("dlib")
gprbuild("-Pada_main -XLIB=static")
run("ada_main")
# delay several seconds so that the timestamps are guaranteed to be different
sleep(4)
# rebuild forcing incorrect compiler option for the subsystem
gprbuild("-f -Psubsystem  -XLIB=dynamic -cargs:ada -gnata")
# relinking the main should happen
gprbuild("ada_main.gpr -XLIB=dynamic")
# and the executable should raise ASSERT_FAILURE
run("ada_main")
gprclean("-r -Pada_main -XLIB=dynamic")
ls("ada_obj/*")
ls("subsystem_obj/*")
ls("dlib/*")
