from gprbuild_utils import *

gprbuild("")
run("obj/main")
gprbuild("-XBUILD=y -f")
run("obj/main")
gprclean("")
ls("obj/*")
