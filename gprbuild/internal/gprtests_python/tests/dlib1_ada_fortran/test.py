from gprbuild_utils import *

gprbuild ("-P ft3 -cargs:ada -gnatws")
env.add_dll_path ("dlib")
run ("fobj3/ess2")
gprclean ("-r -P ft3")
ls ("fobj3/*")
ls ("dlib/*")
ls ("dobj/*")
