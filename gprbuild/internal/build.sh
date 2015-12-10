##Internal build instructions on Windows for cygwin users. Should be
##very similar for Unix users
#/bin/sh
## configure
# e.g.
./configure --prefix=${HOME}/local --build=pentium-mingw32msv

## create a link to the GNAT checkout
# e.g.
/usr/bin/ln -s ${HOME}/repository/gnat gnat_src
make copy_gnat_src
# some more files for building the doc
cp -p gnat_src/gnat_style.css gnat_src/texiplus.tex doc/

## Build
make all

## Build the doc
make doc

## install
make install

## test with the examples
make examples
