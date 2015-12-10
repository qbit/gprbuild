from gprbuild_utils import *

Run (["gprconfig", "-q", "--batch", "-o", "my_conf.cgpr", "--config=Ada", "--config=C"], output=None)
gprbuild ("-f -q -ws --config=my_conf.cgpr prj.gpr", verbose=True, notarget=True)
run ("main")
