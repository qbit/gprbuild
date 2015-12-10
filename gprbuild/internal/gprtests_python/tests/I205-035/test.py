from gprbuild_utils import *

gprbuild ("-q main.gpr -o mms_pct", verbose=True)
gprbuild ("-q mms_pct.gpr -o mms_pct", verbose=True)
run ("mms_pct")
gprclean ("-q mms_pct.gpr", verbose=True)
