from gprbuild_utils import *

gprbuild ("prj1.gpr", notarget=True)
gprbuild ("prj2.gpr", notarget=True)
gprbuild ("prj2.gpr")
