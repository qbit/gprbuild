from gprbuild_utils import *

# No exception should be raised. Proper error message should be displayed.
gprbuild("", output="tmpout")
f = open("tmpout")
for line in f:
   print re.sub(":[0-9]:[0-9]+:", "", line)
f.close()

# No exception should be raised. Proper error message should be displayed.
gprclean("", output="tmpout2")
f = open("tmpout2")
for line in f:
   print re.sub(":[0-9]:[0-9]+:", "", line)
f.close()
