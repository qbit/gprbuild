from gprbuild_utils import *
import os, sys

gprbuild("-q -p sa_lib3.gpr", verbose=False)
gprbuild("-q -p sa_lib4.gpr", verbose=False)
gprbuild("-q -p good/aggl.gpr", verbose=False)
gprbuild("-q -p aggl/aggl.gpr", verbose=False)
