import os,re
from gprbuild_utils import *

os.mkdir('build')
os.chdir('build')

gprbuild ('--relocate-build-tree --root-dir=../obj/deep/ ../main.gpr',
          verbose=True)

os.chdir("..")
