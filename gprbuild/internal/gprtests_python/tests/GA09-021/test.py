from gprbuild_utils import *

gprbuild ("-Pbug.gpr without_dash.adb", output="output.txt")
output_file = open("output.txt")
for line in output_file:
    if "failed" in line:
        print line.rstrip()
gprclean ("-q -Pbug.gpr")
ls ("obj/*")
ls ("bin/*")
