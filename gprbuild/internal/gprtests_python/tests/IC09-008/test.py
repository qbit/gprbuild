from gprbuild_utils import *

gprbuild ("prj.gpr")
sleep (4)
Run (["touch", "main.ads"], output=None)
Run (["touch", "main.adb"], output=None)
Run (["touch", "pkg1.ads"], output=None)
Run (["touch", "pkg2.ads"], output=None)
gprbuild ("-v -m prj.gpr", verbose=True, output="output.txt")
Run (["grep", "different", "output.txt"], output=None)
