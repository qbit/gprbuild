from gprbuild_utils import *

gprbuild ("-vP1 aggr.gpr", verbose=True, output="output.txt")
Run (["grep", "TOTO", "output.txt"], output=None)

os.environ ["TOTO"] = "toto"
gprbuild ("-vP1 aggr.gpr", verbose=True, output="output.txt")
Run (["grep", "TOTO", "output.txt"], output=None)

