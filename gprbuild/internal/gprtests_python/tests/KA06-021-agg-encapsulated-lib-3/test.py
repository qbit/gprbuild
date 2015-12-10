from gprbuild_utils import *
import os, sys

gprbuild("-q -p extlib/extlib.gpr", verbose=False)
gprbuild("-q -p main.gpr", verbose=False)
env.add_dll_path ("extlib/lib")
env.add_dll_path ("lib3")
run("main")
