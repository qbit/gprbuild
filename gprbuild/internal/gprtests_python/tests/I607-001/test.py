from gprbuild_utils import *

add_dll_dir("lib")
gprbuild(["-Puser"])
run("user")
gprclean(["-Puser"])
