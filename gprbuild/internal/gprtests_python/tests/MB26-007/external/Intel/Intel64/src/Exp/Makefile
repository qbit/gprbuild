CC=gcc
CPP=g++

CFLAGS= -pipe -O3 -ffast-math 
LIBCFLAGS=-pipe -O3 -ffast-math

SRC_LIB=	Intel_Dispatcher.c 

LIB=../../uMath/libIExp.a

OBJ_LIB=$(SRC_LIB:.c=.o)


all: $(OBJ_LIB)
	ar cr $(LIB) $(OBJ_LIB)

clean:
	rm -f $(OBJ_LIB)

distclean: clean
	rm -f $(LIB)

%.o:%.c
	$(CC) $(LIBCFLAGS) -c $< -o $@

%.o:%.cpp
	$(CC) $(CFLAGS) -c $< -o $@

