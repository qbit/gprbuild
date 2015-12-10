from gprbuild_utils import *

touch ("output.txt")
rm ("output.txt")
touch ("output.txt")

env.add_path(TEST_DIR)

gprbuild ("-q -P prj.gpr")
gprbuild ("-p -q gprbuildproblem.gpr")
run ("sort_output")
