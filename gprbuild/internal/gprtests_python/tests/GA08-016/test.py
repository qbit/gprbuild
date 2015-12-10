from gprbuild_utils import *

ls('*', 'ls.1')
gprbuild ("--autoconf=./default.cgpr -q", verbose=True)
run ("main")
gprclean ("--autoconf=./default.cgpr -q", verbose=True)
ls('*', 'ls.2')
Run (["diff","ls.1", "ls.2"], output=None)
output = diff('ls.1', 'ls.2')
if output:
    print output
