from gprbuild_utils import *

# Test that gprbuild and gnatmake have the same behavior when files
# on the command line do not belong to the root project
# This is the gprbuild-specific part of the test. One more test in fixedbugs
# tests gnatmake

print "=== gprbuild, main files from same imported project"
gprbuild("default.gpr -c -f imp1.adb imp2.adb", verbose=True)
ls("*.o")
rm("*.o")

# gprbuild allows main files from multiple projects
print "=== gprbuild, main files from two different projects"
gprbuild("default.gpr -c -f root.adb imp1.adb", verbose=True)
ls("*.o")
rm("*.o")
