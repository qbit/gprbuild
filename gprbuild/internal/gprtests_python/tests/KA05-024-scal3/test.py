from gprbuild_utils import *

print "no"
gprbuild ("-q -p -XLS=no shared.gpr", verbose=True)
print "standard"
gprbuild ("-q -p -XLS=standard shared.gpr", verbose=True)
print "full"
gprbuild ("-q -p -XLS=encapsulated shared.gpr", verbose=True)
