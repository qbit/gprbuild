from gprbuild_utils import *

Run (["gnatname", "-Pprj", "-d.", "-da/b", "-da/**", "-da/d", "*.spec", "*.body"], output=None)
gprbuild ("-f -q -Pprj driver.body", verbose=True)
run ("driver")
