from gprbuild_utils import *

# first case
gprbuild("-Pbasic1")
run("main")
ls("main.o")
gprclean("-Pbasic1")

# second case: basic1.gpr is the only project so used by default
gprbuild("main.c")
run("main")
ls("main.o")
gprclean("-Pbasic1")

# third case: compile options on the command line
gprbuild("-Pbasic1 -cargs:c -DXXX")
run("main")
ls("main.o")
gprclean("-Pbasic1")
