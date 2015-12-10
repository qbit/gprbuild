from gprbuild_utils import *

gprbuild(["-Palways_fail", '-v'])
run("main")
gprclean("-Palways_fail")
