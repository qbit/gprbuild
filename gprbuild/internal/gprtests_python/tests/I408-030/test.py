from gprbuild_utils import *

os.environ ["TMPDIR"]=""
gprbuild ("-q -f prj.gpr -dn", verbose=True)
Run (["cat", "GNAT-TEMP-000001.TMP"], output=None)
