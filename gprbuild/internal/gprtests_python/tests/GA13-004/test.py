from gprbuild_utils import *

gprbuild ("-q -P test3_ada_main.gpr -cargs:ada -gnatws")
run ("test3_ada_main")
