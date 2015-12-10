from gprbuild_utils import *
import time

gprbuild("-m -s default.gpr", verbose=True)
time.sleep(3)
touch("main.adb")

print "Should not rebuild anything"
gprbuild("-m -s default.gpr", verbose=True)
