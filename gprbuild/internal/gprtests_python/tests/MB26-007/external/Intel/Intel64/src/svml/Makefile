NAME := ../../uMath/libisvml.a

SRC := svml_interface.c

PATH:=/opt/intel/Compile/:${PATH}

SRC_TEST := test.c

OBJECT:=$(SRC:.c=.o)

OBJ_TEST:=$(SRC_TEST:.c=.o)

CC:=icc

CFLAGS:= -Wall -g -O3 -xSSE3 -restrict

LDFLAGS:=-L../../uMath/

LIBS=-lisvml -lsvml -limf -lIExp

all:$(OBJECT)
	ar cr $(NAME) $(OBJECT)

.c.o:
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -rf $(OBJECT) svml_test *.o

distclean: clean
	rm -f $(NAME) 

test: all 
	gcc -Wall -g -O3 -msse3 -c test.c -o test.o
	gcc test.o $(LDFLAGS) $(LIBS) -o svml_test
