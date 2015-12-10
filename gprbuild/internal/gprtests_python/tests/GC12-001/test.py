from gprbuild_utils import *

gprbuild ("prj.gpr");
gprbuild ("prj.gpr", verbose=True)
sleep (3.5)
touch ("toto.adc")
gprbuild ("prj.gpr", verbose=True)
gprbuild ("prj.gpr", verbose=True)

