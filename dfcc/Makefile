CFLAGS = -Wall -pedantic -g
LDFLAGS = -no-pie
SRCS = $(wildcard *.c)
OBJS1 = $(addprefix .tmp1/,$(SRCS:.c=.o))
OBJS2 = $(addprefix .tmp2/,$(SRCS:.c=.o))

all: dfcc

stage1: $(OBJS1)
	$(CC) $(LDFLAGS) -o $@ $(OBJS1)

stage2: stage1 $(OBJS2)
	$(CC) $(LDFLAGS) -o $@ $(OBJS2)

dfcc: stage2
	cp $< $@

test: stage1 stage2 test/test.c test/extern.c
	./stage1 test/test.c > .tmp1/test.s
	./stage2 test/test.c > .tmp2/test.s
	$(CC) $(CFLAGS) -o .tmp1/extern.o -c test/extern.c
	$(CC) $(CFLAGS) -o .tmp2/extern.o -c test/extern.c
	$(CC) $(LDFLAGS) -o .tmp1/test .tmp1/test.s .tmp1/extern.o
	$(CC) $(LDFLAGS) -o .tmp2/test .tmp2/test.s .tmp2/extern.o
	.tmp1/test
	.tmp2/test

.tmp1/%.o: %.c dfcc.h
	@mkdir -p .tmp1
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
	./stage1 .tmp2/$< > .tmp2/$<.s
	$(CC) $(CFLAGS) -o $@ -c .tmp2/$<.s

clean:
	rm -rf stage1 stage2 dfcc .tmp1 .tmp2

.PHONY: test
