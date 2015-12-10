from gprbuild_utils import *

def check():
    for filename in ("lib1/liblib1.a", "obj/a.o", "obj/b.o", 
		    "obj/c.o", "lib3/liblib3.a", "obj3/a.o",
		    "obj3/b.o"):
        if not os.path.exists(filename):
            print "%s does not exist" % filename
    if os.path.exists("obj3/c.o"):
      print "obj3/c.o exist but should not"

print ("simple lib extension")
gprbuild ("-q -Plib1")
gprbuild ("-q -Plib3")
check()
gprclean ("-q -r -Plib1")
gprclean ("-q -r -Plib3")

print ("externally built lib extension")
gprbuild ("-q -Plib1")
gprbuild ("-q -Plib4")
check()
gprclean ("-q -r -Plib1")
gprclean ("-q -r -Plib4")
