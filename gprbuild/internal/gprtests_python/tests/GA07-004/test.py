from gprbuild_utils import *

gprbuild ("-f prj.gpr -cargs:ada -O2 -cargs -O1", verbose=True, output="output.txt")
f = open('output.txt')
for line in f:
    if line.startswith('ar'):
        print 'ar'
    elif 'ranlib' in line:
        print 'ranlib'
    else:
        print line
f.close()
