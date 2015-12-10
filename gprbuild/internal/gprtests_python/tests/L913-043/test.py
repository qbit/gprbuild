from gprbuild_utils import *
gprbuild ("--config=gnat2why.cgpr -p -P test.gpr --subdirs=gnatprove --restricted-to-languages=ada -k a1.adb", notarget=True , verbose=True);
rm ("gnatprove/b.o");
gprbuild ("--config=gnat2why.cgpr -ws -P test.gpr --subdirs=gnatprove --restricted-to-languages=ada -k a1.adb", notarget=True, verbose=True);
