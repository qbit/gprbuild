from gprbuild_utils import *

gprbuild ("-q", verbose=True)
cd ("obj")
run ("main")
cd ("..")
gprbuild ("-q -XBUILD=y -f", verbose=True)
cd ("obj")
run ("main")
