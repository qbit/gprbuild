from gprbuild_utils import *
import re

# Build the imported project (so that it has something to clean), but not
# the toplevel project (which does not even have its object directory)
gprbuild (["-p", "-Pprj2", "-u"])

# Running gprclean recursively should still clean the imported project)
# gprclean should not complain (ie warning, not error) that the object
# directory is missing for the toplevel project)
gprclean (["-r", "-Pdefault"], output="tmp.out")
f = open("tmp.out")
for line in f:
    print re.sub("default\.gpr:.:[0-9]+: ", "", line)
f.close()

# There should be nothing left in the object directory of the imported
# project
ls ("obj2/*")

