from gprbuild_utils import *

gprbuild ("-q -c prj.gpr -cargs --RTS=sjlj", verbose=True)
gprbuild ("prj.gpr", verbose=True)
