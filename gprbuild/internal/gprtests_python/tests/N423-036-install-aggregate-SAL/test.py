from gprbuild_utils import *

gprbuild ("aggr.gpr")
gprinstall (['-q', '-p', 'aggr.gpr', '--prefix='+os.getcwd()+'/inst'])
gprbuild ("main.gpr")
