from gprbuild_utils import *

example_dir = os.path.join(EXAMPLES_DIR, 'scenarios')
libraries_dir = os.path.join(EXAMPLES_DIR, 'libraries')
mkdir('example')

fileutils.rsync(example_dir, 'example')
fileutils.rsync(libraries_dir, 'libraries')

p = Run(['make', '-C', 'example', 'run'])
for line in p.out.splitlines():
    if 'Done in Ada' in line:
        print line

