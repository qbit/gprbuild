from gprbuild_utils import *

example_dir = os.path.join(EXAMPLES_DIR, 'namings')
mkdir('example')

fileutils.rsync(example_dir, 'example')

Run(['make', '-C', 'example', "--no-print-directory"])
pd = Run(['make', '-C', 'example', "--no-print-directory", 'run'])
for line in pd.out.splitlines():
    if 'up to date' not in line and 'make:' not in line:
        print line
