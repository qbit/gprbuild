from gprbuild_utils import *

# Putting unit names on the command line is not supported
gprbuild("-Pprj driver")

# But putting truncated file names is supported
gprbuild("-Pprj toto")

# Make sure the compilation went fine
run("toto")
