from gprbuild_utils import *

gprbuild ("")
run ("obj/main")
ls ("obj/main.o")
gprclean ("-Pbasic2")
ls ("obj/*")
