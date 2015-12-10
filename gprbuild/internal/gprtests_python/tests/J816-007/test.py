from gprbuild_utils import *

gprbuild ("-q prj.gpr -bargs -O", verbose=True, output="output.txt");
run("filter");
rm ("output.txt");
gprbuild ("-f -q prj.gpr -bargs -O=output.txt", verbose=True);
run("filter");
