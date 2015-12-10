from gprbuild_utils import *

env.add_path(os.getcwd())

gprbuild ("toto.gpr")
gprbuild ("-ws --config=default.cgpr prj.gpr", verbose=True, notarget=True)
gprbuild ("-ws --config=default.cgpr prj.gpr", verbose=True, notarget=True)
