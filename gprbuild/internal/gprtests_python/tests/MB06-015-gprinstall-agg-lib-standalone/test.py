from gprbuild_utils import *
import os, sys

gprbuild("-q -p lib/asa_lib.gpr", verbose=False)
gprinstall(['--prefix='+os.getcwd()+"/inst", "-q", "-p", "lib/asa_lib.gpr"],
           verbose=False)

gprbuild(["-q", "-p", "main.gpr", "-margs",
          "-aP"+os.getcwd()+"/inst/share/gpr"], verbose=False)

env.add_dll_path ("inst/lib/asa_lib")
run("main");
