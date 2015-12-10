from gprbuild_utils import *
import os, sys

def display_files(msg):
    print msg
    flist=[]
    for root, subFolders, files in os.walk("."):
        for file in files:
            ext = os.path.splitext(file)[1]
            if (ext == '.o' and root != '.' and file.find("__") == -1) or ext == '.ali' or ext == '.so' or ext == '.dll' or ext == '.dylib':
                flist.append(os.path.join(root,file))
    for f in sorted(flist):
        # convert \ windows separators to /
        print f.replace('\\', '/').replace('.dll', '.so').replace('.dylib', '.so')

def display_clean():
    for file in sorted(open("clean.out").readlines()):
        print file

os.mkdir ("o1lib")
os.mkdir ("o2lib")
os.mkdir ("a2lib")
os.mkdir ("laggr")

print "=================== RUN1"
gprbuild("aggr.gpr", verbose=True)
gprclean("-r aggr.gpr", verbose=True, output="clean.out")
display_clean()

print "=================== RUN2"
gprbuild("-XLIBRARY_TYPE=relocatable aggr.gpr")
display_files("after compilation")
gprclean("-r -XLIBRARY_TYPE=relocatable aggr.gpr")
display_files("after clean")

print "=================== RUN3"
gprbuild("main.gpr", verbose=True)
run("main")
gprclean("-r main.gpr", verbose=True, output="clean.out")
display_clean()
