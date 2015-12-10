from gprbuild_utils import *

# Test that gprclean supports aggregate projects
# The compilation itself is tested in K520-025, with the same testcase

gprbuild("aggr.gpr", verbose=False)
gprclean("aggr.gpr", verbose=True)
ls("obj/*")   # Make sure there are no remaining files
ls("lib/*")
