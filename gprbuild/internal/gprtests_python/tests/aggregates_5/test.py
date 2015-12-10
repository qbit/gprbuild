from gprbuild_utils import *

# Tests an aggregate project that overrides the project path

gprbuild("aggrko.gpr", verbose=True);

gprbuild("aggr.gpr", verbose=True);
run("tree1/a")
