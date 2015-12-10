from gprbuild_utils import *

gprbuild ("-q -P test2_cpp_main.gpr -cargs:ada -gnatws")
run ("test2_cpp_main")
