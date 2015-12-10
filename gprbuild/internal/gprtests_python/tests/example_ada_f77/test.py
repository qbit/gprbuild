from gprbuild_utils import *

example_dir = os.path.join(EXAMPLES_DIR, 'ada_f77')
mkdir('example')

fileutils.rsync(example_dir, 'example')

Run(['make', '-C', 'example'])
for line in Run(['make', '-C', 'example', 'run']).out.splitlines():
    if 'up to date' not in line and 'make:' not in line:
        print line
