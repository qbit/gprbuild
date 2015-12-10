from gprbuild_utils import *

gprbuild ("prj.gpr --config=test.cgpr", notarget=True, verbose=True, output="output.txt")
f = open('output.txt')
compils = [line for line in f]
compils.sort()
for line in compils:
   print line

sleep (4)
touch ("p3.ads")
gprbuild ("prj.gpr --config=test.cgpr", notarget=True, verbose=True, output="output2.txt")
f = open('output2.txt')
compils = [line for line in f]
compils.sort()
for line in compils:
   print line

