from gprbuild_utils import *

example_dir = os.path.join(EXAMPLES_DIR, 'extended_projects')
subsystems_dir = os.path.join(EXAMPLES_DIR, 'subsystems')
mkdir('example')

fileutils.rsync(example_dir, 'example')
fileutils.rsync(subsystems_dir, 'subsystems')

Run(['make', '-C', 'example'])
for line in Run(['make', '-C', 'example', 'run']).out.splitlines():
    if 'Doing' in line:
        print line
