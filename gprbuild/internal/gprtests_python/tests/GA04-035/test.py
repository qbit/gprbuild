from gprbuild_utils import *

gprbuild ("-q -P test4_cpp_main.gpr -cargs:ada -gnatws")
run ("test4_cpp_main")
gprclean ("-r -P test4_cpp_main.gpr")
