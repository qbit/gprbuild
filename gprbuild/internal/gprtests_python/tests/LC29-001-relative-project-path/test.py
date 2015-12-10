from gprbuild_utils import *

gprbuild ("-p -q dscovr.work/dscovr_agg.gpr");
cd("dscovr.work")
gprbuild ("-p -q dscovr_agg.gpr");
