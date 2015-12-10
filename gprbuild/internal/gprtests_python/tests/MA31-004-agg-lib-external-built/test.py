from gprbuild_utils import *

gprbuild ("-p -q build_ext.gpr");
gprbuild ("-p -q aggl.gpr");
gprbuild ("-p -q main.gpr");
