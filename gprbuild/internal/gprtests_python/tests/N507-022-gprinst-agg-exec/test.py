from gprbuild_utils import *
import os, sys

gprbuild("-q -p agg.gpr", verbose=False)

gprinstall (['-q', '--prefix='+os.getcwd()+"/inst", 'agg.gpr'])

run ("inst/bin/foo")

