from gprbuild_utils import *

print 'gprbuild -c'
gprbuild ("-c prj.gpr", verbose=True);
print 'gprbuild -U'
gprbuild ("-U prj.gpr", verbose=True);
