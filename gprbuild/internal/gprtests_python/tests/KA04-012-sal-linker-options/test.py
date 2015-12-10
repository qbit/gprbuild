from gprbuild_utils import *

p = gprbuild(["-p", "-gnat05", "-XSAL=Yes", "test.gpr", "-v"], output='output.txt')

with open('output.txt') as f:
    result = f.read()
    if '-ltoto' in result:
        print "cannot find -ltoto"
    else:
        print "error, -ltoto not passed to the linker\n" + result
