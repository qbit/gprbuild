from gprbuild_utils import *

# Tests an aggregate project, where aggregates projects are found
# through wildcards
gprbuild("aggr.gpr")
run("tree1/a")
run("tree2/a")
