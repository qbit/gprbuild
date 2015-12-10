from gprbuild_utils import *

gprbuild ("prj.gpr -k -j8")
gprclean ("prj.gpr")
gprbuild ("prj2.gpr -k -j8", output="output.txt")
Run (["grep", " link of", "output.txt"], output="list.txt")
Run (["sort", "list.txt"], output=None)
gprclean ("prj2.gpr")
gprbuild ("prj.gpr -k -j8 -v -vP2", output="output.txt")
Run (["grep", "link process", "output.txt"], output=None)
