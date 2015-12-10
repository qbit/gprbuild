from gprbuild_utils import *

print "standard"
gprbuild ("-q -p -XLS=standard shared.gpr", verbose=True)
print "full"
gprbuild ("-q -p -XLS=full shared.gpr", verbose=True)
print "no"
gprbuild ("-q -p -XLS=no shared.gpr", verbose=True)
