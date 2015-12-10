from gprbuild_utils import *

example_dir = os.path.join(EXAMPLES_DIR, 'matrix')
mkdir('example')

fileutils.rsync(example_dir, 'example')

Run(['make', '-C', 'example'])
for line in Run(['make', '-C', 'example', 'run']).out.splitlines():
    if 'Corresponding' in line:
        print line
