from gprbuild_utils import *

# run main in C calling a routine in Ada that needs elaboration
# there are more sources than needed in the source dir
# the project limits the number of sources to use
gprbuild ("-Pbasic2 -cargs:c -w")
run ("c_main")
ls ("ada_lib.o")
ls ("c_main.o")
# should also check that there is no lib.c
gprclean ("-Pbasic2")
