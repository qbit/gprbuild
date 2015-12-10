from gprbuild_utils import *

gprbuild ("-f -v a.gpr -p", verbose=True, output="output.txt")

f = open('output.txt')
for line in f:
    for pattern in ('g++%s -c' % env.host.os.exeext,
                    'g++%s -shared' % env.host.os.exeext,
                    'g++%s -dynamiclib' % env.host.os.exeext):
        if pattern in line:
            print pattern
