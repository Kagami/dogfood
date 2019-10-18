CFLAGS = -Wall -Werror -pedantic -g -std=c11
LDFLAGS = -no-pie
SRCS = $(wildcard *.c)
OBJS1 = $(addprefix .tmp1/,$(SRCS:.c=.o))
OBJS2 = $(addprefix .tmp2/,$(SRCS:.c=.o))
TSRCS = $(wildcard test/*.c)
TESTS = $(addprefix .tmp2/,$(TSRCS:.c=))

all: stage1

stage1: $(OBJS1)
	$(CC) $(LDFLAGS) -o $@ $(OBJS1)

stage2: $(OBJS2)
	$(CC) $(LDFLAGS) -o $@ $(OBJS2)

test: $(TESTS)
	@echo -e " \e[32mOK"

.tmp1/%.o: %.c dfcc.h
	@mkdir -p .tmp1
	$(CC) $(CFLAGS) -o $@ -c $<

.tmp2/%.o: %.c stage1
	@mkdir -p .tmp2
	@cat ../libdfc/include/*.h dfcc.h $< \
		| grep -v '^#' \
		| sed 's/\bbool\b/_Bool/g' \
		| sed 's/\btrue\b/1/g' \
		| sed 's/\bfalse\b/0/g' \
		| sed 's/\bNULL\b/0/g' \
		| sed 's/INT_MAX/2147483647/g' \
		| sed 's/\berrno\b/*__errno_location()/g' \
		| sed 's/\bva_start\b/__builtin_va_start/g' \
		> .tmp2/$<
	./stage1 .tmp2/$< | $(CC) $(CFLAGS) -o $@ -xassembler -c -

.tmp2/test/%: test/%.c stage1
	@mkdir -p .tmp2/test
	@cat ../libdfc/include/*.h test/test.h $< \
		| grep -v '^#' \
		| sed 's/\bbool\b/_Bool/g' \
		| sed 's/\btrue\b/1/g' \
		| sed 's/\bfalse\b/0/g' \
		| sed 's/\bNULL\b/0/g' \
		| sed 's/INT_MAX/2147483647/g' \
		| sed 's/\berrno\b/*__errno_location()/g' \
		| sed 's/\bva_start\b/__builtin_va_start/g' \
		> .tmp2/$<
	@./stage1 .tmp2/$< | $(CC) $(LDFLAGS) -o $@ -xassembler -
	@$@

clean:
	rm -rf stage1 stage2 dfcc .tmp1 .tmp2

.PHONY: test $(TSRCS)
