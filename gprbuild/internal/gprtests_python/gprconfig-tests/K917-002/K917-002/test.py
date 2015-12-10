from gprconfig_utils import *

create_fake_ada_compiler ("install", "dotnet", "6.1.0w", "3.4.6", True)

os.environ["PATH"] = os.path.join(TEST_DIR, 'install', 'bin')

test_gprconfig_batch (['--target=dotnet', "--config=Ada"])
