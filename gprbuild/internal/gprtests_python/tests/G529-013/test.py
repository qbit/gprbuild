from gprbuild_utils import *

touch ("original/source-proc.adb source-proc.adb")
gprbuild ("-q original/a.gpr")
run ("original/ada_test")
gprbuild ("-q b.gpr")
run ("./ada_test")
