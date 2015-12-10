from gprbuild_utils import *

gprbuild ("agg.gpr");
run("a")
run("b")
gprbuild ("agg.gpr aa bb")
run("aa")
run("bb")
gprbuild ("agg.gpr c");

