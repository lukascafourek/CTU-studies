CC = gcc
CFLAGS = -Wall -g -O2
EXTRA_CFLAGS ?= -fsanitize=address -fno-omit-frame-pointer

factory: factory.c
	$(CC) $(CFLAGS) $(EXTRA_CFLAGS) factory.c -o factory -lpthread

clean:
	rm -f factory
