from gprbuild_utils import *

# first case
gprbuild ("-q")
run ("obj/main")
ls ("obj/main.o")
gprclean ("-Pbasic2")
ls ("obj/*")
