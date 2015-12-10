from gprbuild_utils import *

os.environ ["TMPDIR"] = "."
gprclean ("prj.gpr", verbose=True, notarget=True)

