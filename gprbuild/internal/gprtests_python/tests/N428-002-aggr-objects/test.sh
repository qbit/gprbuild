
rm -fr obj lib zlib/zobj zlib/zlib xml/xobj xml/xlib

gprbuild -q -p xml/xml_dom_build.gpr

rm xml/xobj/dom.o

gprbuild -q -p main.gpr

nm lib/libaggr.a
echo "Symbol whatever should be listed in code.o"
