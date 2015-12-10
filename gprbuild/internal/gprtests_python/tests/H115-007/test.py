from gprbuild_utils import *

gprbuild ("-q gener.gpr");
run ("gener")

gprbuild ("-p -c -q prj.gpr")
gprbuild ("-p prj.gpr", verbose=True, output='output.txt')

f = open('output.txt')

patterns = {'ar cr' : False, 'ar q' : False}

def filter(l):
    for key in patterns:
        if key in l:
            if patterns[key]:
                return None
            else:
                patterns[key] = True
                return key
    return l

for line in f:
    line = filter(line)
    if line is not None:
        print line
