from gprbuild_utils import *

os.environ['LIB'] = ''
gprbuild ("-Pada_main")
run ("ada_main")
# delay so that the timestamps are guaranteed to be different
sleep (4)
# rebuild forcing incorrect compiler option for the subsystem
gprbuild ("-f -Psubsystem -cargs:ada -gnata")
# relinking the main should happen
gprbuild ("ada_main.gpr")
# and the executable should raise ASSERT_FAILURE
run ("ada_main")
gprclean ("-r -Pada_main")
ls ("ada_obj/*")
ls ("subsystem_obj/*")
