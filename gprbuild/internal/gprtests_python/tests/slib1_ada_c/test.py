from gprbuild_utils import *

# now test subsystem as a static library
gprbuild ("-Pada_main -XLIB=static")
run ("ada_main")
# delay so that the timestamps are guaranteed to be different
sleep (4)
# rebuild forcing incorrect compiler option for the subsystem
gprbuild (" -f -Psubsystem  -XLIB=static -cargs:ada -gnata")
# relinking the main should happen
gprbuild ("ada_main.gpr -XLIB=static")
# and the executable should raise ASSERT_FAILURE
run ("ada_main")
gprclean ("-r -Pada_main -XLIB=static")
ls ("ada_obj/*")
ls ("subsystem_obj/*")
ls ("slib/*")
