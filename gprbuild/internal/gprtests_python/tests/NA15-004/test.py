from gprbuild_utils import *

gprbuild ("-q -c prj.gpr main.adb");
gprbuild ("-q -p ada.gpr", verbose=True)
run ("objada/main")
