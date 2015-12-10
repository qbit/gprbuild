from gprbuild_utils import *

import re

gprbuild ("-q prj.gpr", verbose=True, output="tmp.out")
f = open("tmp.out")
for line in f:
    print re.sub("prj\.gpr:.:[0-9]+: ", "", line)

run ("main")
