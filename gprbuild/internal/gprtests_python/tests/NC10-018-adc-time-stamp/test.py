
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
p = subprocess.Popen(["gprslave", "--port=0", "-v", "-j4"], stdout=subprocess.PIPE)

try:
    # read line to find the gprslave port
    line = p.stdout.readline()
    if line[0:8] == 'GPRSLAVE':
        port = line[line.find(':')+1:].strip()
    else:
        printfl("first line is: '" + line + "'")

    # launch compilation
    os.chdir("..")

    gprbuild('--distributed=localhost:'+port+ ' -p -j2 build/main.gpr')

    gprbuild('--distributed=localhost:'+port+ ' -p -j2 build/main.gpr',
             verbose=True, output='gprbuild.out')

    # terminate the slave
    p.kill()

    # check main
    run("build/main");

    printfl(open("gprbuild.out").readlines())

except:
    p.kill()
