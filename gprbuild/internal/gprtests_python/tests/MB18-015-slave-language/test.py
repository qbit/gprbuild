
from gprbuild_utils import *
from time import sleep
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
    sleep(0.5)
    # read line to find the gprslave port
    line = p.stdout.readline()
    if line[0:8] == 'GPRSLAVE':
        port = line[line.find(':')+1:].strip()
    else:
        printfl("first line is: '" + line + "'")

    # launch compilation
    os.chdir("..")

    gprbuild('--distributed=localhost:'+port+ ' -p -q -j2 build/main.gpr')

    # check main
    run("build/main");

    # terminate the slave
    sleep(0.5)
    p.kill()

except:
    sleep(0.5)
    p.kill()
