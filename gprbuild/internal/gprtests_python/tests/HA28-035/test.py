from gprbuild_utils import *

gprbuild ("-q prj.gpr pkg.ads", verbose=True, output="output.txt")

f = open('output.txt')

for line in f:
    if 'cannot' in line:
        print line
f.close()

