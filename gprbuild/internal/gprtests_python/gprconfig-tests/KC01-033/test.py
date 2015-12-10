from gprconfig_utils import *
import os

create_fake_ada_compiler("install", "i686-pc-linux-gnu", "6.1.0w", "3.4.6",
                         create_symlink=True)

os.environ["PATH"] = os.path.join(TEST_DIR, 'install', 'bin')

test_gprconfig_interactive("s", ['--target=i686-pc-linux-gnu'])
