from gprbuild_utils import *

gprbuild ("-q -P mixed_lang_project.gpr")
run ("cpp_main")
gprclean ("-r -q mixed_lang_project.gpr")
