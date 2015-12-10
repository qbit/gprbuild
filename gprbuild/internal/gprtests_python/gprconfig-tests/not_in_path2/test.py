from gprconfig_utils import *

create_fake_ada_compiler("install", "i686-pc-linux-gnu", "6.1.0w", "3.4.6")
create_fake_ada_compiler("install2", "i686-pc-linux-gnu", "6.1.0w", "3.4.6")

os.environ["PATH"] = os.path.join('install', 'bin') + os.pathsep \
    + os.path.join('install2', 'bin')

test_gprconfig_batch(['--config', 'Ada,,native,' +
                      os.path.join('install2', 'bin'),
                      '--target=i686-pc-linux-gnu'])

test_gprconfig_interactive("s", ['--config', 'Ada,,native,' +
                                 os.path.join('install2', 'bin'),
                                 '--target=i686-pc-linux-gnu'])
