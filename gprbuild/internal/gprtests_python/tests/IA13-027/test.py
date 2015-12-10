from gprbuild_utils import *

gprbuild (["default.gpr", "bar.ads", "foo.ads"], output="output.txt")

f = open('output.txt')

for line in f:
    if 'cannot' in line:
        print line
f.close()


