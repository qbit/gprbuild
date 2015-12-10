from gprbuild_utils import *

os.environ['LIB'] = ''

gprbuild ("-Pc_main -cargs:c -w")
run ("c_main")
# rebuild forcing incorrect compiler option for the subsystem
sleep (2)
gprbuild ("-f -Psubsystem -cargs:c -DXXX")
# rebuilding the main should happen
gprbuild ("c_main.gpr")
# and the executable should report an error about XXX defined
run ("c_main")
gprclean ("-r -Pc_main")
#check that all object dirs are empty
ls ("c_obj/*")
ls ("subsystem_obj/*")
