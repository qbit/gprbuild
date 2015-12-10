import os
from gprbuild_utils import *

# For default

gprbuild (['-XOS=UNIX', '--subdirs=sub/dir', 'prj/prj.gpr'])
gprinstall (['--prefix='+os.getcwd()+"/inst", '--subdirs=sub/dir',
             '-XOS=UNIX', 'prj/prj.gpr'],
            output='tmp.out', verbose=True)

if os.path.isfile (os.getcwd() + "/inst/lib/prj/pck1.ali"):
    print ("OK (ali)")
else:
    print ("Not installed")

if os.path.isfile (os.getcwd() + "/inst/lib/prj/libprj.a"):
    print ("OK (lib)")
else:
    print ("Not installed")
