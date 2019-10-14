CFLAGS = -g
LDFLAGS = -no-pie
SRCS = $(wildcard *.c)
OBJS = $(addprefix .tmp/,$(SRCS:.c=.o))
OBJS2 = $(addprefix .tmp2/,$(SRCS:.c=.o))

all: dfcc

dfcc: $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $(OBJS)

dfcc2: dfcc $(OBJS2)
	$(CC) $(LDFLAGS) -o $@ $(OBJS2)

test: dfcc test/test.c test/extern.c
	./dfcc test/test.c > .tmp/test.s
	$(CC) $(CFLAGS) -o .tmp/extern.o -c test/extern.c
	$(CC) $(LDFLAGS) -o .tmp/test .tmp/test.s .tmp/extern.o
	.tmp/test

test2: dfcc2 test/test.c test/extern.c
	./dfcc2 test/test.c > .tmp2/test.s
	$(CC) $(CFLAGS) -o .tmp2/extern.o -c test/extern.c
	$(CC) $(LDFLAGS) -o .tmp2/test .tmp2/test.s .tmp2/extern.o
	.tmp2/test

.tmp/%.o: %.c dfcc.h
	@mkdir -p .tmp
	$(CC) $(CFLAGS) -o $@ -c $<

.tmp2/%.o: %.c dfcc.h
	@mkdir -p .tmp2
	cat ../libdfc/stdio.h dfcc.h $< \
		| grep -v '^#' \
		| sed 's/\bbool\b/_Bool/g' \
		| sed 's/\berrno\b/*__errno_location()/g' \
		| sed 's/\btrue\b/1/g' \
		| sed 's/\bfalse\b/0/g' \
		| sed 's/\bNULL\b/0/g' \
		| sed 's/INT_MAX/2147483647/g' \
		> .tmp2/$<
	./dfcc .tmp2/$< > .tmp2/$<.s
	$(CC) $(CFLAGS) -o $@ -c .tmp2/$<.s

clean:
	rm -rf dfcc dfcc2 .tmp .tmp2

.PHONY: test
