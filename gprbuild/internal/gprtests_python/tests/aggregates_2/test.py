from gprbuild_utils import *

# Test various error cases in aggregate projects

gprbuild("default.gpr")
gprbuild("aggr1.gpr")
gprbuild("aggr2.gpr")
gprbuild("aggr3.gpr")
gprbuild("aggr4.gpr")
gprbuild("aggr5.gpr")
