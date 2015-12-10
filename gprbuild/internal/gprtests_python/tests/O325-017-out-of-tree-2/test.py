import os,re
from gprbuild_utils import *

def check(file):
    if os.path.exists(file):
        print("%s present" % file)
    else:
        print("%s not found" % file)

os.mkdir('build')
os.chdir('build')

gprbuild ('--relocate-build-tree -XODIR=' + os.getcwd()+'/obj' + ' ../main.gpr', verbose=True)

run ("obj/main")

os.chdir("..")

# check build
print("--- build")
check("build/obj/auto.cgpr")
check("build/obj/main.o")
check("build/obj/b__main.o")
check("build/obj/b__main.ali")

# let's clean and check that it is correct

print("--- clean")

os.chdir('build')
gprclean ('--relocate-build-tree -XODIR=' + os.getcwd()+'/obj' + ' ../main.gpr')
os.chdir('..')

check("build/obj/auto.cgpr")
check("build/obj/main.o")
