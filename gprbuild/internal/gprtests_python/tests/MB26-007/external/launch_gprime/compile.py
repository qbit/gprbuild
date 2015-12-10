import sys
import os.path

dependencies_only = False
if sys.argv[ 1 ].startswith("-d"):
    long_filename = sys.argv[ 1 ][2:]
    dependencies_only = True
else:
    long_filename = sys.argv[ 1 ]

short_filename = os.path.split( long_filename )[ 1 ]
object_filename = "%s.o"%short_filename

if dependencies_only:
    dependency_string = "%s: %s"%( object_filename, short_filename )
    #the dependency file is created on stdout
    print dependency_string
else:
    os.system("ld -r -b binary %s -o %s"%( short_filename, object_filename ) )
