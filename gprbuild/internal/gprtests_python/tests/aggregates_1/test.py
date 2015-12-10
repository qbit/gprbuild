from gprbuild_utils import *

# Tests that we can load an aggregate project where two trees have
# sources with a common base name
gprbuild("aggr.gpr")
run("tree1/a")
run("tree2/a")
