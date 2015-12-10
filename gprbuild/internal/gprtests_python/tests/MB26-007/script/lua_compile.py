#the purpose of this python script is to convert a lua script from the lua
#directory
# (e.g. src/lua/script.lua ) into an object file with 'good' symbols.
#
# The problem is that when invoked through gprbuild, ld receives an absolute
# pathname, which will in turn generate a developer-dependent symbol, which
# will cause the pragma import statement to fail.  So what we do is turn it
# into something 'manageable', by turning the absolute pathname into a relative
# one, which is not developer-dependent.
#
# Granted, this is not squeaky-clean, but according to the folks at Adacore,
# it's not that bad either.

import sys
import os

if __name__ == "__main__":
    #print "ARGS: ", sys.argv
    #the -d flag generates a dependency file for gprbuild.
    dependencies_only = False
    filename = None
    #we either receive "-dfile.lua" ( dependency mode )
    if sys.argv[ 1 ].startswith( "-d" ):
       dependencies_only = True
       filename = sys.argv[ 1 ][2:]
    #or plain "file.lua"
    else:
       dependencies_only = False
       filename = sys.argv[ 1 ]

    #we build ../src/lua/filename.lua from /absolute/path/to/lua
    #short_name = filename.lua
    short_name =  os.path.split( filename )[ 1 ] 
    relative_name  = os.path.join( "..", "src", "lua", short_name )

    #we will generate filename_lua.o
    object_name = short_name.replace('.','_') + ".o"
    dependency_name =  short_name.replace('.','_') + ".d"



    if dependencies_only:
        #the string looks like
        #luafile_lua.o: ../src/lua/luafile.lua
        dependency_string = "%s: %s"%( object_name, relative_name) 

        #WARNING : the .d file is generated from stdout
        #this means that anything that gets printed for debugging reasons
        #will end up in the dep file, potentially creating weird invalid
        #syntax bugs, which can only be pinpointed using gprbuild -v
        print dependency_string
    else:
        s='ld -r -b binary %s -o %s'%( relative_name, object_name )
        os.system( s )

