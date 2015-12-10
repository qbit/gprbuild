from gprbuild_utils import *

env.add_path(os.getcwd())

p = Run(["gnatmake", "-q", "mydriver.adb"])

gprbuild("--config=rc.cgpr -ws prj.gpr", notarget=True)

gprbuild("--config=rc2.cgpr -ws prj.gpr", notarget=True)
