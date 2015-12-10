
from gprbuild_utils import *
from time import sleep
import subprocess
import os, sys

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
    sleep(0.5)
    # read line to find the gprslave port
    line = p.stdout.readline()
    if line[0:8] == 'GPRSLAVE':
        port = line[line.find(':')+1:].strip()
    else:
        printfl("first line is: '" + line + "'")

    # launch compilation
    os.chdir("../build")

    gprbuild('--distributed=localhost:'+port+ ' --slave-env=me -p -q -j1 main.gpr')

    # check main
    run("main");

    # terminate the slave
    sleep(0.5)
    p.kill()
except:
    sleep(0.5)
    p.kill()

# and double check that the obj for the slave has been created

if os.path.exists("../slave/me/main/mobj"):
    printfl("slave/mobj exists");
if os.path.exists("../slave/me/main/mlib"):
    printfl("slave/mlib exists");
