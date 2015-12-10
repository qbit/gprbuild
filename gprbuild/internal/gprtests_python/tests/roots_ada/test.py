from gprbuild_utils import *

os.environ["LANG"] = ""
gprbuild("")
ls ("obj/*.o")
run ("obj/bidule")
run ("obj/foo")
gprclean("")
ls ("obj/*")
gprbuild ("truc.adb")
ls ("obj/*.o")
run ("obj/truc")
gprclean ("truc.adb")
ls ("obj/*")
