from gprconfig_utils import *

## Test that when using multiple --config switches, a compatible set of
## compilers is selected

create_fake_ada_compiler ("install", "i686-pc-linux-gnu", "3.16", "4.2.0")
create_fake_c_compiler ("install", "i686-pc-linux-gnu", "4.2.0")
create_fake_ada_compiler ("install2", "i686-pc-linux-gnu", "6.1.0", "4.2.0")
create_fake_c_compiler ("install3", "i686-pc-linux-gnu", "4.1.0")

os.environ['PATH'] = os.path.join('install', 'bin') + os.pathsep \
    + os.path.join('install2', 'bin') + os.pathsep \
    + os.path.join('install3', 'bin')

test_gprconfig_batch (['--db', TEST_DIR, '--config=Ada,,native',
                       '--config=C',
                       '--target', 'i686-pc-linux-gnu'])
