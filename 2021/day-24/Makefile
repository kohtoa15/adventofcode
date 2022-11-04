CFLAGS = -g -Wall

bin: src/main.c src/fsio.h src/alu.h src/alu.c src/fsio.c
	clang $(CFLAGS) src/fsio.h src/fsio.c src/alu.h src/alu.c src/main.c

clean:
	rm -rf a.out*

all: clean bin

run: bin
	./a.out test1
