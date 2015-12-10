from gprbuild_utils import *

# Should report an error that "ff" does't exist
print "==== -u -Plib_test test.adb ff"
gprbuild("-u -Plib_test test.adb ff")
print "==== -Plib_test ff test.adb"
gprbuild("-Plib_test ff test.adb")
print "==== -Paggr ff"
gprbuild("-Paggr ff")
print "==== -u -Paggr ff"
gprbuild("-u -Paggr ff")

# But no error reported if the file belongs to an aggregated projects
print "==== -u -Paggr unit1.adb"
gprbuild("-f -u -Paggr unit1.adb", verbose=True, output="output.txt")
for line in sorted(open('output.txt').readlines()):
    print line
print "==== -u -Paggr test.adb"
gprbuild("-f -u -Paggr test.adb", verbose=True, output="output.txt")
for line in sorted(open('output.txt').readlines()):
    print line
