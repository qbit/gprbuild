from gprbuild_utils import *

gprbuild ("-f -q prj.gpr", verbose=True)
run ("main")
gprbuild ("-f -q prj.gpr -XGNATBIND_PATH=/toto/gnatbind", verbose=True)
