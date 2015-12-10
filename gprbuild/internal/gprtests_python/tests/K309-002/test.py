from gprbuild_utils import *

# Should compile both "Unit1" and "Unit1.Child", since they are both in the
# same file.
gprbuild("-u -Plib_test unit1.adb", verbose=True)
