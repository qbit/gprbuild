from gprbuild_utils import *

gprbuild ("-f -q prj.gpr", verbose=True)
gprbuild ("-f -q prj.gpr --indirect-imports", verbose=True)
gprbuild ("-f -q prj.gpr --no-indirect-imports", verbose=True)
gprbuild ("-f -q prj.gpr --no-indirect-imports --indirect-imports", verbose=True)
