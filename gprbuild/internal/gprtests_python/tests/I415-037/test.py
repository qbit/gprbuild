from gprbuild_utils import *

p = Run (["gprbuild", "--version"])
if 'GPRBUILD' in p.out and '86' in p.out and 'linux' in p.out:
    print 'x86-linux'
else:
    print p.out

