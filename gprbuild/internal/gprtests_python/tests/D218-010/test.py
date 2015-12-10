from gprbuild_utils import *

gprbuild ("-q -P prj -XBUILD=default")
run ("main")
gprclean ("-q -r -P prj -XBUILD=default")
gprbuild ("-q -P prj -XBUILD=advanced")
run ("main")
gprclean ("-q -r -P prj -XBUILD=advanced")
