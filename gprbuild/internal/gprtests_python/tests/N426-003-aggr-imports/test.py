from gprbuild_utils import *

gprbuild("-q -p xml/xml_dom_build.gpr")

os.remove("xml/obj/dom.o");

gprbuild("-q -p aggr.gpr")
