
from gprbuild_utils import *
import os, sys
import subprocess

def printfl(mes):
    print(mes)
    sys.stdout.flush()

port=''

# run slave into it's own subdirectory
os.mkdir("slave")
os.chdir("slave")
p = subprocess.Popen(["gprslave", "--port=0", "-v", "-j4"],
                     stdout=subprocess.PIPE)

try:
    # read line to find the gprslave port
    line = p.stdout.readline()
    if line[0:8] == 'GPRSLAVE':
        port = line[line.find(':')+1:].strip()
    else:
        printfl("first line is: '" + line + "'")

    # launch compilation
    os.chdir("../build")

    gprbuild('--distributed=localhost:' + port +
             ' --slave-env=build -p -q -j2 main.gpr')

    # check main
    run("main");

    if os.path.exists("../slave/build/main"):
        printfl("OK, build env present")
    else:
        printfl("ERROR: build env not found")

    gprclean('--distributed=localhost:' + port +
             ' --slave-env=build -q main.gpr')

    if os.path.exists("../slave/build/main"):
        printfl("ERROR: build/gen still there")
    else:
        printfl("OK, removed")

    # terminate the slave
    p.kill()

except:
    p.kill()
