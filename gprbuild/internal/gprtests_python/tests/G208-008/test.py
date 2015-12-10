from gprbuild_utils import *

gprbuild ("-f -q -P prj.gpr", output="tmpout")
f = open("tmpout")
for line in f:
    print re.sub(":[0-9]:[0-9]+:", "", line)
f.close()
gprbuild ("-f -q -P prj.gpr -aPimp")
gprbuild ("-f -q -P prj.gpr", output="tmpout2")
f = open("tmpout2")
for line in f:
    print re.sub(":[0-9]:[0-9]+:", "", line)
f.close()
gprbuild ("-f -q -P prj.gpr -aPimp", output="tmpout")
#gnat list -aPimp -P prj.gpr
gprclean ("-q -r -aPimp -P prj.gpr")
