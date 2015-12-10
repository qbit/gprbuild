from gprbuild_utils import *

gprbuild ("-f -q -XProduct=A -Psimulation main.ada")
rm ("main.bexch")
sleep (2)
gprbuild ("-XProduct=C -Psimulation main.ada", verbose=True)
gprclean ("-q -XProduct=A -Psimulation")
gprclean ("-q -XProduct=C -Psimulation")

gprbuild ("-q -XProduct=A -Psimulation main.ada")
rm ("main.bexch")
sleep (2)
gprbuild ("-XProduct=B -Psimulation main.ada", verbose=True)
gprclean ("-q -XProduct=A -Psimulation")
gprclean ("-q -XProduct=B -Psimulation")

gprbuild ("-q -XProduct=A -Psimulation main.ada")
rm ("main.bexch")
sleep (2)
gprbuild ("-XProduct=D -Psimulation main.ada", verbose=True)
gprclean ("-q -XProduct=A -Psimulation")
gprclean ("-q -XProduct=D -Psimulation")
