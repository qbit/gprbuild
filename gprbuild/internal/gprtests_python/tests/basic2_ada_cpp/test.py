from gprbuild_utils import *

gprbuild ("-Pexcept")
run ("obj/ada_main")
run ("obj/cpp_main")
gprclean ("")
ls ("obj/*")
