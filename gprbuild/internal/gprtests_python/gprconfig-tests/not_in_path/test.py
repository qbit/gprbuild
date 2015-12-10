from gprconfig_utils import *

create_fake_ada_compiler ("install", "i686-pc-linux-gnu", "6.1.0w", "3.4.6")
# The compiler is not in the PATH, but should still be found
test_gprconfig_batch (['--config', 'Ada,,native,%s/install/bin' % TEST_DIR,
                       '--target=i686-pc-linux-gnu'])

os.environ["PATH"] = os.path.join(TEST_DIR, 'install')
test_gprconfig_interactive ("s", ['--config',
                             'Ada,,native,%s/install/bin' % TEST_DIR,
                             '--target=i686-pc-linux-gnu'])
