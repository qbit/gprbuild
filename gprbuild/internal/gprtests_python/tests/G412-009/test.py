from gprbuild_utils import *

env.add_path(os.getcwd())

gprbuild ("cbnd.gpr")
gprclean ("-c cbnd.gpr")
Run (["ls", "-1"], output="ls.1")
gprbuild ("-q -ws --config=default.cgpr prj.gpr", verbose=True, notarget=True)
run ("main")
gprclean ("-q --config=default.cgpr prj.gpr", verbose=True, notarget=True)
Run (["ls", "-1"], output="ls.2")
Run (["diff", "ls.1", "ls.2"], output=None)
