from gprbuild_utils import *

gprbuild ("-Pnamings -XC1=case1")
run ("main-case1")
gprbuild ("-Pnamings -XC1=case2")
run ("main-case2")
gprbuild ("-Pnamings -XC1=case3")
run ("main-case3")

gprclean ("-Pnamings -XC1=case1")
ls ("obj-case1/*")
gprclean ("-Pnamings -XC1=case2")
ls ("obj-case2/*")
gprclean ("-Pnamings -XC1=case3")
ls ("obj-case3/*")
