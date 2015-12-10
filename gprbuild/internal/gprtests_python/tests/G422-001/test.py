from gprbuild_utils import *

gprbuild ("-q foo.gpr -Xlanguage=french")
run ("hello")
sleep (6)
gprbuild ("-q foo.gpr -Xlanguage=english")
run ("hello")
gprclean ("-q foo.gpr -Xlanguage=french")
gprclean ("-q foo.gpr -Xlanguage=english")
