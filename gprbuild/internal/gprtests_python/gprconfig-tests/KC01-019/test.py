from gprconfig_utils import *
import os

env.store()

# Add only ada_object_path to check it is taken into account
create_fake_ada_compiler("install", "i686-pc-linux-gnu", "6.1.0w", "3.4.6",
                         create_symlink=False, create_ada_object_path=True)

os.environ["PATH"] = os.path.join(TEST_DIR, 'install', 'bin')
test_gprconfig_interactive("s", ['--target=i686-pc-linux-gnu'])

env.restore()

print ""
print ""
print "====== Do we remove duplicates ?"

# Add both create_symlink and ada_object_path to check we are correctly
# removing duplicates
create_fake_ada_compiler("install", "i686-pc-linux-gnu", "6.1.0w", "3.4.6",
                         create_symlink=True, create_ada_object_path=True)

os.environ["PATH"] = os.path.join(TEST_DIR, 'install', 'bin')
test_gprconfig_interactive("s", ['--target=i686-pc-linux-gnu'])
