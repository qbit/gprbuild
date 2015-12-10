from gprbuild_utils import *

gprbuild ("-f -q -k -p prj1.gpr -cargs -O -gnatn")
gprbuild ("-f -q prj1.gpr --no-split-units p.adb -cargs -O -gnatn")
gprbuild ("-f -q prj1.gpr --no-split-units q.adb -cargs -O -gnatn")
