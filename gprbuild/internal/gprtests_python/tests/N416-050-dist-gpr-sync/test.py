
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

printfl("start gprslave")

p = subprocess.Popen(["gprslave", "--port=0", "-v", "-j4"],
                     stdout=subprocess.PIPE)

try:
    printfl("wait for gprslave port")
    # read line to find the gprslave port
    line = p.stdout.readline()
    if line[0:8] == 'GPRSLAVE':
        port = line[line.find(':')+1:].strip()
    else:
        printfl("first line is: '" + line + "'")

    printfl("gprslave port found")
    # launch compilation
    os.chdir("..")
    os.chdir("build")

    # change last access time of p2/pck10.adb in the past
    f='p2/src/pck10.adb'
    st = os.stat(f)
    atime = st.st_atime # access time
    mtime = st.st_mtime # modification time

    new_mtime = mtime - (3*3600) # new modification time

    # modify the file timestamp
    os.utime(f,(atime,new_mtime))

    sleep(0.1)
    printfl("P1-1")
    os.chdir("p1")
    gprbuild('--distributed=localhost:'+port+ ' -p  -j1 main.gpr')
    run("main");
    os.chdir("..")

    sleep(0.1)
    printfl("P2-1")
    os.chdir("p2")
    gprbuild('--distributed=localhost:'+port+ ' -p  -j1 main.gpr')
    run("main");
    os.chdir("..")

    sleep(0.1)
    printfl("P1-2")
    os.chdir("p1")
    gprclean('main.gpr')
    gprbuild('--distributed=localhost:'+port+ ' -p  -j1 main.gpr')
    run("main");
    os.chdir("..")

    sleep(0.1)
    printfl("P2-2")
    os.chdir("p2")
    gprclean('main.gpr')
    gprbuild('--distributed=localhost:'+port+ ' -p  -j1 main.gpr')
    run("main");
    os.chdir("..")

    # terminate the slave
    printfl("Kill gprslave")
    sleep(0.5)
    p.kill()

except:
    sleep(0.5)
    p.kill()
