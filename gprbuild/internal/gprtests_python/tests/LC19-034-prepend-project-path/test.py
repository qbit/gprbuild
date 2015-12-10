from gprbuild_utils import *

os.environ ["ADA_PROJECT_PATH"] = "./sal.main/"
gprbuild ("-q dscovr_agg.gpr");
