from gprbuild_utils import *

gprbuild ("prj.gpr")
gprbuild ("prj.gpr --target=toto --config=auto.cgpr", notarget=True)
gprbuild ("prj.gpr -XTARG=true --config=auto.cgpr", notarget=True)

