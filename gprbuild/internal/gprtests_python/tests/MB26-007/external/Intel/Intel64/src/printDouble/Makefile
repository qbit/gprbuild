SRC=	Main.c

LIB_SRC= libPrintDouble.c	

all:
	gcc -c -g3 -W -Werror -ansi $(LIB_SRC) -o libPrintDouble.o
	ar cr ../../uMath/libPrintDouble.a	libPrintDouble.o

test: all
	gcc -g3 -W -Werror -ansi $(SRC) ../../uMath/libPrintDouble.a -o test_printDouble

clean:
	rm -rf *.o test_printDouble

distclean: clean
	rm -rf libPrintDouble.a
