CFLAGS = -D_POSIX_C_SOURCE=200809L -Wall -Wpedantic -Werror -g -std=c11
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

fulltest: clean stage2 test

.tmp1/%.o: %.c dfcc.h
	@mkdir -p .tmp1
	$(CC) $(CFLAGS) -o $@ -c $<

.tmp2/%.o: %.c stage1
	@mkdir -p .tmp2
	@cat ../libdfc/include/*.h dfcc.h $< > .tmp2/$<
	./stage1 -o .tmp2/$<.s .tmp2/$<
	$(CC) $(CFLAGS) -o $@ -c .tmp2/$<.s

.tmp2/test/%: test/%.c stage1
	@mkdir -p .tmp2/test
	@cat ../libdfc/include/*.h test/test.h $< > .tmp2/$<
	@./stage1 -o .tmp2/$<.s .tmp2/$<
	@$(CC) $(LDFLAGS) -o $@ .tmp2/$<.s
	@$@

clean:
	rm -rf stage1 stage2 .tmp1 .tmp2

.PHONY: test $(TSRCS)
