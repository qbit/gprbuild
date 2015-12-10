from gprbuild_utils import *
import os

gprbuild("-q -p xml/xml_dom_build.gpr")

os.remove("xml/obj/dom.o");

gprbuild("-q -p main.gpr")

for dirname, dirnames, filenames in os.walk('lib'):
    for filename in filenames:
        if filename not in ["libaggr.a", "pck.ali", "pck2.ali"]:
            print ("Wrong filename in lib " + filename)
