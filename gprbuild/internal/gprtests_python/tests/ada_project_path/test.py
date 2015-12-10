from gprbuild_utils import *

env.add_search_path("ADA_PROJECT_PATH", os.path.join(os.getcwd(), 'config'))

gprbuild ("-Pprj -XBUILD=debug")
run("obj-debug/hello")
gprclean("-Pprj -XBUILD=debug")
ls("obj-debug/*")
gprbuild("-Pprj -XBUILD=prod")
run("obj-prod/hello")
gprclean("-Pprj -XBUILD=prod")
