from gprbuild_utils import *

os.environ ["TMPDIR"] = ""
os.chmod ("obj", 0555)
Run (["gprconfig", "-o", "default.cgpr", "--batch", "--config=Ada", "-q"], output=None)
gprbuild ("prj.gpr -ws --config=default.cgpr", verbose=True, notarget=True)
os.chmod ("obj", 0777)
