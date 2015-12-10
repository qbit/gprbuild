from gprbuild_utils import *

example_dir = os.path.join(EXAMPLES_DIR, 'subsystems')
mkdir('example')

fileutils.rsync(example_dir, 'example')

for line in Run(['make', '-C', 'example', 'run']).out.splitlines():
    if ' in ' in line or line in ('./ada_main', './c_main'):
        print line
