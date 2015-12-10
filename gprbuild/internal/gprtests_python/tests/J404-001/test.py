from gprbuild_utils import *

os.environ ["CPP_TESTING"] = "false"
gprbuild("-q gvd_cpp.gpr", verbose=True)
