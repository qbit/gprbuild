from gprbuild_utils import *

gprbuild ("-q -P animals.gpr -cargs:ada -gnatws")
run ("main")
