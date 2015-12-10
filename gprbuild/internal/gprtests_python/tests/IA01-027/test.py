from gprbuild_utils import *
fpic = ""
if env.host.os.name in ("linux", "solaris", "hp-ux", "freebsd"):
    fpic = " -cargs -fPIC"

gprbuild ("--unchecked-shared-lib-imports -q -Pmain1" + fpic)
add_dll_dir ("lib1_dir")
run ("main1_o/main")
gprclean ("--unchecked-shared-lib-imports -r -q  -Pmain1")

gprbuild ("--unchecked-shared-lib-imports -q -Pmain2" + fpic)
add_dll_dir ("lib3_dir")
run ("main2_o/main")
gprclean ("--unchecked-shared-lib-imports -r -q  -Pmain2")


