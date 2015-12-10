from gprbuild_utils import *

print "gprbuild -q prj.gpr"
gprbuild ("-q prj.gpr", verbose=True)
print "gprbuild -q prj.gpr"
gprbuild ("-q prj.gpr", verbose=True)
print "gprbuild -f -q prj2.gpr"
gprbuild ("-f -q prj2.gpr", verbose=True)
print "gprbuild -f -q prj3.gpr"
gprbuild ("-f -q prj3.gpr", verbose=True)
print "gprbuild -f -q prj4.gpr"
gprbuild ("-f -q prj4.gpr", verbose=True)
print "gprbuild -f -q prj5.gpr"
gprbuild ("-f -q prj5.gpr", verbose=True)
print "gprbuild -f -q prj6.gpr"
gprbuild ("-f -q prj6.gpr", verbose=True)
print "gprbuild -f --no-direct-imports direct/prj.gpr -k -q"
gprbuild ("-f --no-indirect-imports direct/prj.gpr -k -q", verbose=True)
