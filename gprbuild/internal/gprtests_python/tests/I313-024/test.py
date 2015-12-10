from gprbuild_utils import *

gprbuild ("tools.gpr")
gprbuild ("-f -q -s windres.gpr", verbose=True)
gprbuild ("-s windres.gpr", verbose=True)
gprbuild ("-f -q -s dlltool.gpr", verbose=True)
gprbuild ("-s dlltool.gpr", verbose=True)
ls ("ssl.a")
ls ("aws.coff")
