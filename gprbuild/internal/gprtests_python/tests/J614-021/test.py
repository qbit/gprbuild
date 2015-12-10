from gprbuild_utils import *

gprbuild ("prj.gpr --source-info=sourceinfo.txt");
run ("bin/main");
gprclean ("-r prj.gpr");
gprbuild ("prj.gpr --source-info=sourceinfo.txt");
run ("bin/main");
gprclean ("-r prj.gpr");

