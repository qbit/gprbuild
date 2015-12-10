from gprbuild_utils import *

gprbuild ("-eL -P prj -q -f")
gprclean ("-eL -P prj -q")
