from gprbuild_utils import *

ls ("*", output="list.1")
gprbuild ("-q prj.gpr", verbose=True)
gprclean ("-q prj.gpr", verbose=True)
ls ("*", output="list.2")
Run (["sort", "list.1"], output="ls1")
Run (["sort", "list.2"], output="ls2")
Run (["diff", "ls1", "ls2"], output=None)

# Do another compilation and run main and main2
# Note that run can generate some files on cross target that
# we don't want to filter.
gprbuild ("-q prj.gpr", verbose=True)
run ("main")
run ("main2")
