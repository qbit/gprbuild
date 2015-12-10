from gprconfig_utils import *

create_fake_ada_compiler("install", "i686-pc-linux-gnu", "6.1.0w", "3.4.6")

test_gprconfig_batch(['--config', 'Ada,,native,' +
                      os.path.join(TEST_DIR, 'install', 'bin'),
                      '--target=i686-pc-linux-gnu'])
