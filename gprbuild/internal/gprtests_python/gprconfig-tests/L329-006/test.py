from gnatpython.fileutils import mkdir
from gprconfig_utils import *
from gprbuild_utils import ROOT_DIR
import os
import re

create_fake_ada_compiler("install", "powerpc-elf", "6.1.0w", "3.4.6",
                         runtimes=["native", "zfp"],
                         create_ada_object_path=True)

os.environ["PATH"] = os.path.join(TEST_DIR, 'install', 'bin')
pwd = os.getcwd()
mkdir("%s/rtssubdir" % pwd)

test_gprconfig_interactive("s", ['--target=powerpc-elf',
                                 '--batch',
                                 '--config=ada,,%s/rtssubdir' % pwd])

generated = file("output.gpr").read() \
        .replace(pwd, "<pwd>") \
        .replace(ROOT_DIR, "<root>")
print re.sub(r'--.*\n','',generated)
