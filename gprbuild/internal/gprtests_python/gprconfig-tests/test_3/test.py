from gprconfig_utils import *

create_fake_ada_compiler ("install", "i686-pc-linux-gnu", "6.1.0w", "3.4.6")

os.environ['PATH'] = os.path.join(TEST_DIR, 'install', 'bin')
test_gprconfig_batch (['--config', 'Ada,,native',
                       '--config', 'C', '--target=i686-pc-linux-gnu'])
