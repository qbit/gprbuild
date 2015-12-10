from gprbuild_utils import *

cd ("tac")
cd ("com")
gprbuild ("-P stans_tac -q")
gprclean ("-r -q -P stans_tac.gpr")
