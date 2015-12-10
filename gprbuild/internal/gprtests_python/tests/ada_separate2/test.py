from gprbuild_utils import *

gprbuild (["-Ptest", "-XOS=unix"])
run ("obj/main")
gprclean (["-XOS=unix"])
ls ("obj/*")
gprbuild (["-Ptest", "-XOS=alpha_vms"])
run ("obj/main")
gprclean (["-XOS=alpha_vms"])
ls ("obj/*")
gprbuild (["-Ptest", "-XOS=itanium_vms"])
run ("obj/main")
gprclean (["-XOS=itanium_vms"])
ls ("obj/*")
