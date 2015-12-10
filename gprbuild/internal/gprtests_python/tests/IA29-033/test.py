from gprbuild_utils import *

Run (["ln", "-s", "test_lib", "link"], output=None)
gprbuild ("link/B.gpr")
os.chmod ("test_lib/objB/B.lexch", 0555)
gprbuild ("-eL test_lib/A.gpr")
