from gprconfig_utils import *

## Test that a language can have spaces in its name (H320-012)
create_fake_ada_compiler ("install", "i686-pc-linux-gnu", "6.1.0w", "3.4.6")
os.environ['PATH'] = os.path.sep + 'bin' + os.pathsep \
    + os.path.join(os.getcwd(), 'install', 'bin')

test_gprconfig_batch (['--config=Lang with spaces'])
