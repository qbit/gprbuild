from gprbuild_utils import *

gprbuild ("-f prj.gpr", verbose=True, output="output.txt")
f = open('output.txt')

for l in f:
    if 'ar cr' in l:
        print 'ar cr'
    else:
        print l

run ("main")
