ZLIB=zlib-1.2.3
ZLIBGPR=zlib.gpr
# On Windows Platform there is a return carriage added , so we have to remove it
PLATFORM=`gcc -dumpmachine | sed -e 's/\r//g'`
OUTPUT=builds
PREFIX=${PWD}/${OUTPUT}/${PLATFORM}
LIBPATH=${OUTPUT}/lib
TESTS=tests

init:
	mkdir -p ${PREFIX}

zlib: init
	cd ${ZLIB} && ./configure --prefix=${PREFIX}
	cd ${ZLIB} && make
	cd ${ZLIB} && make install
	cd ${ZLIB}/contrib/ada && gnatmake -P zlib.gpr -XPlatform=${PLATFORM}
	#cd ${PREFIX}/lib && ar rc libadazlib.a ../../../${ZLIB}/*.o
	#cp ${ZLIB}/contrib/ada/zlib-thin.ads ${PREFIX}/include

zlib-tests:  
	cd ${TESTS}/builds && gnatmake -gnat05  -I../../${ZLIB}/contrib/ada ../src/t_zlib_01.adb -bargs -static -aO${PREFIX}/lib -largs -lz 

zlib-runtests: zlib-tests
	./${TESTS}/builds/t_zlib_01      

clean:
	cd ${ZLIB} && make clean
	cd ${ZLIB}/contrib/ada && gnatclean -P ${ZLIBGPR} -XPlatform=${PLATFORM}
	gnatclean -P ./tests/src/tests.gpr -XPlatform=${PLATFORM}
	rm -f ${PREFIX}/lib/libz.a

install:
	cp ${PREFIX}/include/*.ads ../src

all: init zlib zlib-tests
