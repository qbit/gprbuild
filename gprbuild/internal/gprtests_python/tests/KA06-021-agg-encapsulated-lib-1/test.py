from gprbuild_utils import *
import os, sys

gprbuild("-q -p main.gpr", verbose=False)
run("main");
