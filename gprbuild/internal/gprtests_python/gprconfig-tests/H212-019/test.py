from gprconfig_utils import *

# Same test as test_3, but with a null entry in the PATH (H212-019)
create_fake_ada_compiler ("install", "i686-pc-linux-gnu", "6.1.0w", "3.4.6")
env.add_path(os.path.sep + 'bin')
env.add_path(os.path.join(TEST_DIR, 'install', 'bin'))
test_gprconfig_batch  (['--config', 'Ada,,native',
                        '--config', 'C', '--target=i686-pc-linux-gnu'])
