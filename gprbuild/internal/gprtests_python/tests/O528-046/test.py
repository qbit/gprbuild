from gprbuild_utils import *

gprbuild ("prj.gpr")
gprls    ("-P prj.gpr --target=" + env.target.triplet)
sleep (4)
touch ("pkg.adb")
gprls    ("-P prj.gpr --target=" + env.target.triplet)
cp("pkg.adb_mod", "pkg.adb")
touch("pkg.adb")
gprls    ("-P prj.gpr --target=" + env.target.triplet)
rm("pkg.ali")
gprls    ("-P prj.gpr --target=" + env.target.triplet)

