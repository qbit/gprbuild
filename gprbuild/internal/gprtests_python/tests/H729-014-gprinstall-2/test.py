import os,re
from gprbuild_utils import *

gprbuild ('prj/lib2.gpr')

gprinstall (['-p', '-r', '-m', '--prefix='+os.getcwd()+"/inst",
             'prj/lib2.gpr'], output='tmp.out', verbose=False)

gprbuild (['-aPinst/share/gpr', 'main.gpr'])

run ("main")

gprinstall (['--prefix='+os.getcwd()+"/inst", '--list'],
            output='list.out', verbose=False)
gprinstall (['--prefix='+os.getcwd()+"/inst", '--list', '--stat'],
            output='stat.out', verbose=False)

gprinstall (['--prefix='+os.getcwd()+"/inst", '--uninstall', 'lib2.gpr'],
            output='tmp.out', verbose=False)
gprinstall (['--prefix='+os.getcwd()+"/inst", '--uninstall', 'lib1.gpr'],
            output='tmp.out', verbose=False)

content = open ("list.out").readlines()
out=False
cs=[]
for l in content:
    if l[0:7] == 'List of':
        out=True
    elif out == True:
        cs = cs + [l]

cs.sort()
for l in cs:
        print l.replace('\\', '/')

cs = []
content = open ("stat.out").readlines()
out=False
for l in content:
    if l[0:7] == 'List of':
        out=True
    elif out == True:
        cs = cs + [l]
cs.sort()
for l in cs:
    print re.sub("[0-9] Kb", "X Kb", l.replace('\\', '/'))

if os.path.exists("inst"):
    print "ERROR: inst still there"
else:
    print "OK, removed"
