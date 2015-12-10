NAME=gmalloc
LIBNAME=lib$(NAME).so
CC=gcc
CFLAGS=-W -Wall -fPIC

SOURCE=	internal_alloc.c\
		gmalloc.c


OBJECT=$(SOURCE:.c=.o)

TEST_SRC=main.c
LIBS=-ldl -rdynamic


all: $(OBJECT)
	$(CC) $(CFLAGS) -shared $(OBJECT) -o $(LIBNAME) $(LIBS)



.c.o:
	$(CC) $(CFLAGS) -c $< -o $@


clean:
	rm -f $(OBJECT) *~

distclean: clean
	rm -f $(LIBNAME)

test:
	gcc -W -Wall -g3 main.c -o $(NAME)_test 

deploy:all
	cp $(LIBNAME) ~/GPUPrime/bin/
