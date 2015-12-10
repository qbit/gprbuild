from gprbuild_utils import *

gprbuild ("prj.gpr -j4", verbose=True, output="output.txt")
f = open('output.txt')
compils = [line for line in f if 'compilation' in line]
compils.sort()
for line in compils:
    print line
