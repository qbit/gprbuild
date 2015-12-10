from gprbuild_utils import *

gprbuild("", output="tmpout")
f = open("tmpout")
for line in f:
   print re.sub(":[0-9]:[0-9]+:", "", line)
f.close()

gprclean("", output="tmpout2")
f = open("tmpout2")
for line in f:
   print re.sub(":[0-9]:[0-9]+:", "", line)
f.close()
