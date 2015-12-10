from gprbuild_utils import *
import os.path

env.add_path(os.getcwd())

gprbuild ("-q prj.gpr --create-map-file", verbose=True);
if os.path.isfile('main.map'):
    run('main')
    rm("main.map")
else:
    print "--create-map-file does not create main.map"

gprbuild ("-f -q prj.gpr --create-map-file=toto.map", verbose=True);
if os.path.isfile('toto.map'):
    run('main')
    rm("toto.map")
else:
    print "--create-map-file=toto.map does not create toto.map"

gprbuild ("-f -q prj1.gpr", verbose=True);
if os.path.isfile('main.map'):
    run('main')
    rm("main.map")
else:
    print "--create-map-file in Builder does not create main.map"

gprbuild ("-f -q prj2.gpr", verbose=True);
if os.path.isfile('toto.map'):
    run('main')
    rm("toto.map")
else:
    print "--create-map-file=toto.map in Builder does not create toto.map"

gprbuild ("prj.gpr --create-map-file=");
