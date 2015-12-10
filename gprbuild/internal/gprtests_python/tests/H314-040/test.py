from gprbuild_utils import *

gprbuild ("-p -P tests/sampleIDLs/phaSensorDepthData/phaSensorDepthData-samples.gpr -f -c -b -q -ws -cargs:ada -gnatws", verbose=True)

