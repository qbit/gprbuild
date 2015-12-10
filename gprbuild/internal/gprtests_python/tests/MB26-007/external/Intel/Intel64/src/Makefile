dirs:=Exp svml

all:
	$(foreach dir, $(dirs), cd $(dir) && make; cd -; )

clean:
	$(foreach dir, $(dirs), cd $(dir) && make clean; cd -; )

test:
	cd svml && make test; ./svml_test; cd -;
