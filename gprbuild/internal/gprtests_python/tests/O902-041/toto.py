import re
import os

Run (["gprbuild", "prj.gpr"])
with open ("pkg.ali", 'rb') as f:
    lines = f.read().splitlines()
newlines = []
r = re.compile("^A.*")
added = False
for l in lines:
    newlines.append(l)
    if not added and r.match(l):
        newlines.append("A --RTS=toto")
        added = True

with open("pkg.ali", 'wb') as f:
    f.write('\n'.join(newlines) + '\n')
ali = os.stat("pkg.ali")
obj = os.stat("pkg.o")
os.utime("pkg.ali", (ali.st_atime, ali.st_mtime - 1))
os.utime("pkg.o", (obj.st_atime, ali.st_mtime - 1))

Run (["gprbuild", "prj.gpr"])

