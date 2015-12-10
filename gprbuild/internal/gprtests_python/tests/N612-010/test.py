from gprbuild_utils import *
from subprocess import PIPE

result=Run (["make"], error=PIPE)
print(result.err)

