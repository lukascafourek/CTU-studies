CC=gcc
EXTRA_CFLAGS=
CFLAGS=-Wall$(if $(EXTRA_CFLAGS), $(EXTRA_CFLAGS))

all: forkpipe nsd

forkpipe: forkpipe.c
	$(CC) $(CFLAGS) forkpipe.c -o forkpipe

nsd: nsd_main.o nsd.o nd.o
	$(CC) $(CFLAGS) nsd_main.o nsd.o nd.o -o nsd

nsd_main.o: nsd_main.c
	$(CC) $(CFLAGS) -c nsd_main.c

nsd.o: nsd.c nsd.h
	$(CC) $(CFLAGS) -c nsd.c

nd.o: nd.c nd.h
	$(CC) $(CFLAGS) -c nd.c

clean:
	rm -f forkpipe nsd *.o
