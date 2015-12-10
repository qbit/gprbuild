from gprconfig_utils import *

create_fake_ada_compiler ("install", "x86-lynxos", "6.1.0w", "3.4.6")
test_gprconfig_error ("""Error: no set of compatible compilers was found
Invalid configuration specified with --config
""",
                      ['--config', 'Ada,,,%s/install/bin' % TEST_DIR,
                       '--target=x86-lynxos'])
