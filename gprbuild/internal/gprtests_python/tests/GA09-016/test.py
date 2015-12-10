from gprbuild_utils import *
from gnatpython.ex import Run

# This test makes sure the gnatbind used is the one coming from the GNAT
# installation adn not the first one found in the PATH.

Run(["gcc", "-o", "gnatbind", "gnatbind.c"])
env.add_path(os.getcwd())
Run(["gnatbind"])
gprbuild ("-q prj.gpr")
