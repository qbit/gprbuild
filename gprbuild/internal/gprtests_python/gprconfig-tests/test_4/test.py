from gprconfig_utils import *

create_fake_ada_compiler ("install", "pentium-mingw32msv", "6.1.0w", "3.4.6")
os.environ["PATH"] = os.path.join(TEST_DIR, 'install')
test_gprconfig_batch (['--config',
                       'Ada,,native,%s/install/bin' % TEST_DIR,
                       '--target=i686-pc-mingw32'])
