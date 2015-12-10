from gprbuild_utils import *

gprbuild ("-q -p prj.gpr", verbose=True)
os.chmod ("prj.lexch", 0333)
gprbuild ("-q prj.gpr", verbose=True)
