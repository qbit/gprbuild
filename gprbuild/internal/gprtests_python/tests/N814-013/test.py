from gprbuild_utils import *

gprbuild ("-c reproducer.gpr")
sleep (4)
gprbuild ("-c reproducer.gpr", verbose=True)
sleep (4)
touch ("header.h")
gprbuild ("-c reproducer.gpr", verbose=True)
