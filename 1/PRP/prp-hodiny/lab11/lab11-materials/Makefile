all: main binary linked-list

binary: binary.c
	clang -Wall -pedantic -g binary.c -o bin

linked-list: linked-list.c
	clang -Wall -pedantic -g linked-list.c -o list

main: main.c
	clang -Wall -pedantic -g main.c -o main

clean:
	rm main
	rm bin
	rm list