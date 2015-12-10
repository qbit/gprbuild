from gprconfig_utils import *

create_fake_ada_compiler ("install", "i686-pc-linux-gnu", "6.1.0w", "3.4.6")

# A link to the current directory. This simulates the /usr/bin/X11 link
# to /usr/bin on recent linux distributions, and ensures we do not show
# duplicate names in the interactive menu
os.symlink(os.path.join('install', 'bin'),
           os.path.join('install', 'bin', 'X11'))

os.environ["PATH"] = os.path.join(TEST_DIR, 'install', 'bin', 'X11') \
    + os.pathsep + os.path.join(TEST_DIR, 'install', 'bin')

test_gprconfig_interactive ("s", ['--config', 'Ada,,native',
                                  '--target=i686-pc-linux-gnu'])
