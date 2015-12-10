from gprbuild_utils import *

# Tests that an aggregated library project is correctly build.
# This also check that the missing directories in aggregated projects
# are created as needed with "-p"

gprbuild("-p aggr.gpr")
run("tree1/a")
ls("tree2/lib/libtest.a")
