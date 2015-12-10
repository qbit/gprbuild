from gprbuild_utils import *

gprbuild ("-f -v -vP2 -j4 prj.gpr", verbose=True, output="output.txt")
Run (["grep", "bind process", "output.txt"], output=None)
gprbuild ("-f -v -vP2 -j4 -k prj.gpr -XGNATBIND_PATH=/toto/gnatbind", verbose=True, output="output.txt")
Run (["grep", "binding of", "output.txt"], output="list.txt")
Run (["sort", "list.txt"], output=None)

