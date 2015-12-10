from gprbuild_utils import *

gprbuild ("build.gpr")
run ("build")
gprbuild ("prj.gpr")
gprbuild ("-f prj.gpr")
Run (["nm","lib/libprj.a"], output="nm.txt")
Run (["grep", " T ", "nm.txt"], output="grep.txt")
Run (["sort", "grep.txt"], output=None)

