from gprbuild_utils import *

gprbuild ("-q prj.gpr", verbose=True)
gprclean ("-q -r prj.gpr", verbose=True)
gprbuild ("-q --unchecked-shared-lib-imports prj.gpr", verbose=True)
gprclean ("-q -r --unchecked-shared-lib-imports prj.gpr", verbose=True)
