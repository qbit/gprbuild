from gprbuild_utils import *
import re
import os

gprbuild ("prj.gpr")

sleep(3.5)

with open ("pkg.ali", 'rb') as f:
    lines = f.read().splitlines()
newlines = []
r = re.compile("^A.*")
added = False
for l in lines:
    newlines.append(l)
    if not added and r.match(l):
        newlines.append("A --RTS=dummy")
        added = True

with open("pkg.ali", 'wb') as f:
    f.write('\n'.join(newlines) + '\n')
touch("pkg.ali")
touch("pkg.o")
gprbuild ("prj.gpr")

