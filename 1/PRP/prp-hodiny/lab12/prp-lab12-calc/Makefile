CFLAGS+=  -pedantic -Wall -std=c99 -g
TARGET=calculator

all: $(TARGET) lib

OBJS=$(patsubst %.c,%.o,$(wildcard *calc.c))
LIBS=$(patsubst %.c,lib%.so,$(wildcard *calc.c))

$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) main.c $(OBJS) -o $@

$(OBJS): %.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

$(LIBS): lib%.so: %.c
	$(CC) $(CFLAGS) -fPIC -shared $< -o $@

lib: $(LIBS)

clean:
	$(RM) $(TARGET) $(LIBS) $(OBJS)

.PHONY: clean
