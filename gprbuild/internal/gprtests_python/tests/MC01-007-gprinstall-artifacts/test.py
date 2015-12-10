import os
from gprbuild_utils import *

def cat(file):
    print()

# For default

gprbuild (['-p', 'prj.gpr'])
gprinstall (['-q', '-p', '-m', '--prefix='+os.getcwd()+"/inst", 'prj.gpr'],
            output='tmp.out', verbose=True)
gprinstall (['-q', '-p', '-m', '--prefix='+os.getcwd()+"/inst",
             '--install-name=sec', 'prj.gpr'], output='tmp.out', verbose=True)

print (open('tmp.out').readlines())

print (open("inst/css/js.css").readlines())
print (open("inst/css/web.css").readlines())
print (open("inst/doc/README").readlines())
print (open("inst/doc/userguide.html").readlines())
print (open("inst/doc/userguide_index.html").readlines())
print (open("inst/share/example/README").readlines())

m=open("inst/share/gpr/manifests/prj").readlines()

# remove the sha-1
ms=[]
for l in m[1:]:
    ms = ms + [l[33:]]

# print sorted files to ensure consistent output
for l in sorted(ms):
    print (l)
