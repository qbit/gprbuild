from gprbuild_utils import *

print "=== bad1.gpr"
gprbuild ("-q -P bad1.gpr")
print "=== bad2.gpr"
gprbuild ("-q -P bad2.gpr")
print "=== bad3.gpr"
gprbuild ("-q -P bad3.gpr")
print "=== good1.gpr"
gprbuild ("-f -P good1.gpr", verbose=True)
sleep (1)
print "=== good1.gpr"
gprbuild ("-P good1.gpr", verbose=True)
