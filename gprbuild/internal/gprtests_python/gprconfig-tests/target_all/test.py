from gprconfig_utils import *

rm ('install', True)
rm ('install2', True)

create_fake_ada_compiler ("install", "i686-pc-linux-gnu", "6.1.0w", "3.4.6")
create_fake_ada_compiler ("install2", "sparc-solaris", "6.1.0w", "3.4.6")

os.environ["PATH"] = os.path.join(TEST_DIR, 'install', 'bin') + os.pathsep \
    + os.path.join(TEST_DIR, 'install2', 'bin')
test_gprconfig_interactive ("s", ['--target=all'])
