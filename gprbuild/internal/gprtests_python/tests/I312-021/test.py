from gprbuild_utils import *

env.add_path(TEST_DIR)

gprbuild ("toto.gpr");
gprbuild ("-q -p generate.gpr", verbose=True)
