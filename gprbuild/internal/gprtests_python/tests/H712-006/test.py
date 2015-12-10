from gprbuild_utils import *

gprbuild ("build_pkgs.gpr")
run ("build_pkgs")
gprbuild ("-q main.gpr -p", verbose=True)
add_dll_dir ("lib")
run ("main")
gprclean ("-q -r main.gpr", verbose=True)
