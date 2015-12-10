from gprbuild_utils import *

gprbuild ("-f -q -P prj.gpr")
print "no -o"
ls ("bin/*")
rm ("bin/*")
gprbuild ("-f -q -P prj.gpr -o main.axe")
print "-o main.axe"
ls ("bin/*")
rm ("bin/*")
gprbuild ("-f -q -P prj.gpr -o main")
print "-o main"
ls ("bin/*")
rm ("-f bin/*")
