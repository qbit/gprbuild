from gprbuild_utils import *

gprbuild ("prj.gpr -ws")
Run (["ldd", "main"], output="output.txt")

f = open('output.txt')
if 'lib64' in f.read():
    print 'lib64'
f.close()
