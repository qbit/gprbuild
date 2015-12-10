from gprbuild_utils import *
import shutil

dname = "aaaaaaaaaaaaaaaaaaaa"

n_subpaths = min(7, (fileutils.max_path() - len(os.getcwd()) - 50) / (len(dname) + 1))
mkdir(os.path.join (*[dname] * n_subpaths))

shutil.copytree(dname, "bbbbbb")
shutil.copytree(dname, "cccccc")
shutil.copytree("cccccc", os.path.join("imp", "cccccc"))
gprbuild("-q -P prj")
run("main")
gprclean("-r -q prj.gpr")
rm(dname, True)
rm("bbbbbb", True)
rm("cccccc", True)
rm( "imp/cccccc", True)
