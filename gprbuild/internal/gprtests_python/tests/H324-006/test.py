from gprbuild_utils import *

gprbuild ("-q prj.gpr", verbose=True, output="gprbuild.out")

with open("gprbuild.out") as out:
    for line in out:
        if line.startswith("prj.gpr:"):
            print line.split(" ", 1)[1] # Remove prj.gpr:N:M
        else:
            print line
