from gprbuild_utils import *
from gprconfig_utils import *

gprbuild ("prj.gpr")
gprbuild ("prj.gpr --config=auto.cgpr", verbose=True)
gprconfig ("--batch -o toto.cgpr --config=ada -q")
gprbuild ("prj.gpr --config=toto.cgpr", verbose=True, notarget=True)

