from gprbuild_utils import *

gprbuild ("-f -q -c prj.gpr", verbose=True)
sleep (4)
touch ("main.ali")
gprbuild ("-c prj.gpr", verbose=True)

