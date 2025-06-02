CC = gcc
CFLAGS = -Wall -g -O2
EXTRA_CFLAGS ?= -fsanitize=address -fno-omit-frame-pointer

prod-cons: prod-cons.c
	$(CC) $(CFLAGS) $(EXTRA_CFLAGS) prod-cons.c -o prod-cons -lpthread

clean:
	rm -f prod-cons
