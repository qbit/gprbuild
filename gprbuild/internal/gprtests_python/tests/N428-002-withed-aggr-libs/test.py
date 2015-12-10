from gprbuild_utils import *
import os

gprbuild ("aws.gpr")

gprinstall (['-p', '-q', '--prefix='+os.getcwd()+'/inst', 'aws.gpr'])

gprbuild ("main.gpr")
run ("main")

