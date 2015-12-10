from gprbuild_utils import *
from gnatpython import ex

ex.Run(["gnatmake", "-q", "toto.adb"])
gprbuild ("-q prj.gpr")
run ("main")
