from gprbuild_utils import *

cp("y.adb1", "y.adb")
gprbuild ("-Pmain -f -q -p")
sleep (5)

print "should relink main"
touch ("x.ads")
gprbuild ("-Pmain -q -c -b")
gprbuild ("-Pmain -l", verbose=True)
sleep (5)

print "should not relink main"
touch ("y.ads")
gprbuild ("-Pmain -q -c -b")
gprbuild ("-Pmain -l", verbose=True)
sleep (5)

print "should not relink main"
touch ("z.ads")
gprbuild ("-Pmain -c -b")
gprbuild ("-Pmain -l", verbose=True)
add_dll_dir("lib")
run ("main")
sleep (5)

print "should not relink main"
cp("y.adb2", "y.adb")
touch ("y.adb");
gprbuild ("-Pmain -c -b")
gprbuild ("-Pmain -l", verbose=True)
run ("main")
