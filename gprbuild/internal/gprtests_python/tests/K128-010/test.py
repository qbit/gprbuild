from gprbuild_utils import *

# Test that the name of the language in the Switches attribute is
# case insensitive

gprbuild(["prj1.gpr", "-u", "a.adb"], verbose=True)
gprbuild(["prj2.gpr", "-u", "b.adb"], verbose=True)
