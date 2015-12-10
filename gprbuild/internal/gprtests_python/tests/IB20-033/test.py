from gprbuild_utils import *

gprbuild ("-f --no-object-check -c prj.gpr -cargs:Ada -gnatc -cargs:C -fsyntax-only", verbose=True, output="output.txt")
f = open('output.txt')
compils = [line for line in f]
compils.sort()
# First print the -fsyntax-only command line
for line in compils:
    if '-fsyntax-only' in line:
        words = line.split()
        print words[0], words[-1]
# Send print all other lines
for line in compils:
    if not '-fsyntax-only' in line:
        print line
gprbuild ("--no-object-check -c prj.gpr -cargs:Ada -gnatc -cargs:C -fsyntax-only", verbose=True)
gprbuild ("--no-object-check -c prj.gpr pkg2.ads -cargs:Ada -gnatc -cargs:C -fsyntax-only", verbose=True)
gprclean ("-q prj.gpr", verbose=True)
