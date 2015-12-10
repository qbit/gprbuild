from time import sleep
from gprbuild_utils import *
import os, sys
import subprocess

port=''

def printfl(mes):
    print(mes)
    sys.stdout.flush()

def make_package(n):
    header = """
        package Func%s is
            procedure Bla;
        end Func%s;
    """%(n, n)

    body = """
    with ADA.Text_IO;
    package body Func%s is
        procedure Bla is
        begin
            Ada.Text_IO.Put_Line( "Hello world");
        end Bla;
    end Func%s;
    """%(n, n )

    file("build/src/func%s.ads"%n , "w").write(header)
    file("build/src/func%s.adb"%n , "w").write(body)

def generate(n):
    f = file("build/src/main.adb", "w")
    for i in xrange(n):
        make_package(i)
        f.write("with Func%s;\n"%i )
    f.write( "procedure Main is \n" )
    f.write("begin\n")
    f.write("   null;\n")
    f.write("end Main;")

os.mkdir("build/src")

generate(100)

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
    os.chdir("../build")

    p1 = subprocess.Popen(["gprbuild", "--distributed=localhost:"+port, "-p", "-j1", "main.gpr"], stdout=subprocess.PIPE)

    sleep(1)

    p2 = subprocess.Popen(["gprbuild", "--distributed=localhost:"+port, "-p", "-j1", "main.gpr"], stdout=subprocess.PIPE)

    # check that p2 is failing as using the same build environment
    for k in xrange(2):
        line = p2.stdout.readline()
        if line[0:18] == 'build environment ':
            printfl("P2 has failed")
            break
        line = p1.stdout.readline()
        if line[0:18] == 'build environment ':
            printfl("P1 has failed")
            break

    # terminate the slave and both builder

    sleep(0.1)
    p.kill()
    p1.kill()
    p2.kill()
except:
    sleep(0.1)
    p.kill()
    p1.kill()
    p2.kill()
