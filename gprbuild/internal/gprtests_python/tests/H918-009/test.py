from gprbuild_utils import *

gprbuild ("-p -q -P imp/imp.gpr")
gprbuild ("-p -q --subdirs=sub -P prj.gpr")
run ("obj/sub/main")
gprclean ("-q -r imp/imp.gpr")
rm ("imp/lib obj", True)
