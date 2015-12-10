import os,re
from gprbuild_utils import *

def check(file):
    if os.path.exists(file):
        print("%s present" % file)
    else:
        print("%s not found" % file)

os.mkdir('build')
os.chdir('build')

gprbuild ('--relocate-build-tree ../main.gpr')

gprinstall (['--relocate-build-tree',
             '-p', '-r', '--prefix='+os.getcwd()+"/../inst", '../main.gpr'],
            output='tmp.out', verbose=False)

run ("main")

os.chdir("..")

if os.path.exists("inst"):
    print("OK: inst present")
else:
    print("ERROR, cannot find installation")

# check build
print("--- build")
check("build/auto.cgpr")
check("build/main.o")
check("build/prj/obj1/pck1.o")
check("build/prj/lib1/libLone.a")
check("build/prj/obj2/pck2.o")
check("build/prj/lib2/libLtwo.a")

# check install
print("--- install")
check("inst/share/gpr/main.gpr")
check("inst/share/gpr/lib1.gpr")
check("inst/share/gpr/lib2.gpr")
check("inst/lib/lib1/pck1.ali")
check("inst/lib/lib2/pck2.ali")
check("inst/lib/lib1/libLone.a")
check("inst/lib/lib2/libLtwo.a")

# let's clean and check that it is correct

os.chdir('build')
gprclean ('--relocate-build-tree -r ../main.gpr')
os.chdir('..')

print("--- clean")
check("build/auto.cgpr")
check("build/main.o")
check("build/prj/obj1/pck1.o")
check("build/prj/obj2/pck2.o")
