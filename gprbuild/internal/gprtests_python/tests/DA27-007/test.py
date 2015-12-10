from gprbuild_utils import *
from gnatpython import ex

gprbuild ("-q -Psimple")
ex.Run(["gnatmake", "-q", "dela"])
run ("dela")
touch ("simple_cpp")
touch ("interface.ads")
gprbuild ("-q -Psimple")
run ("cpp_main")
gprclean ("-r -q simple.gpr")
ex.Run(["gnatclean","-q", "dela"])
