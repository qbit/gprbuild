from gprbuild_utils import *

def grep_auto_cgpr(pattern):
    with open("auto.cgpr") as f:
        for line in f:
            if pattern in line:
                print line

gprbuild ("prj1.gpr", verbose="True", notarget="True", output="toto.txt")
grep_auto_cgpr ("Target")

gprbuild ("prj2.gpr", verbose="True", notarget="True", output="toto.txt")
grep_auto_cgpr ("Target")
