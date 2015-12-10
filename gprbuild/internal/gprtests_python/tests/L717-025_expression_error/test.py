from gprbuild_utils import *

gprbuild ("prj.gpr");
os.environ["PATH"] = '.:' + os.environ["PATH"]
gprbuild ("prj.gpr");
