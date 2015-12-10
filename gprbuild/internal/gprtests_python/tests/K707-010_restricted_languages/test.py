from gprbuild_utils import *

print("Ada only")
gprbuild("prj.gpr --restricted-to-languages=Ada", verbose=True);
print("C only")
gprbuild("prj.gpr --restricted-to-languages=C", verbose=True);
print("C++ only")
gprbuild("prj.gpr --restricted-to-languages=C++", verbose=True);
print("Ada again")
gprbuild("prj.gpr --restricted-to-languages=Ada", verbose=True);
print("C again")
gprbuild("prj.gpr --restricted-to-languages=C", verbose=True);
print("cleaning")
gprclean ("prj.gpr");
print("building main");
gprbuild("prj.gpr");
print("running");
run("main");
print("cleaning")
gprclean ("prj.gpr");
print("Ada and C");
gprbuild("prj.gpr --restricted-to-languages=Ada,C", verbose=True, output="output.txt");
for line in sorted(open('output.txt').readlines()):
    print line


