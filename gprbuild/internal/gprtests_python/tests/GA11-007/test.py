from gprbuild_utils import *

gprbuild ("-q prj.gpr", verbose=True, output="tmpout")
f = open("tmpout")
for line in f:
    print re.sub(":[0-9]:[0-9]+:", "", line)
f.close()
