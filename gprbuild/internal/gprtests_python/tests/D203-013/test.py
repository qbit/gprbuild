from gprbuild_utils import *

gprbuild ("-q -P project2")
run ("My_Main_C_file")
gprclean ("-q -r project2.gpr")
