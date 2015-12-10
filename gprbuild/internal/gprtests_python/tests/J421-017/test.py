from gprbuild_utils import *

os.environ ["GNAT_FILE_NAME_CASE_SENSITIVE"] = "0"
gprbuild ("-q -P MGED/prj.gpr", verbose=True);
run ("MGED/GNAT/obj/main");
gprclean ("-q -r -P MGED/prj.gpr", verbose=True);
