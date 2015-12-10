from gprbuild_utils import *

gprbuild ("-p -q -Psubsystem -XLIB=none -cargs:c -DXXX", verbose=True)
gprbuild ("-p -q -Pc_main -XLIB=none", verbose=True)
cd ("subsystem_obj")
rm ("lib.o")
cd ("..")
sleep (2)
gprbuild ("-p -Pc_main -XLIB=none", verbose=True)
run ("c_main")
gprclean ("-q -r -Pc_main -XLIB=none", verbose=True)
