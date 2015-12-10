from gprbuild_utils import *

gprbuild ("-vP2 prj.gpr", verbose=True, output="output.txt")

f = open('output.txt')
for line in f:
    if 'Mapping' not in line and 'Override' not in line and \
       'pkg_.ada' in line:
        index = line.find('File')
        if index >= 0:
            print line[index:]
        elif not 'Path =' in line:
            print line
