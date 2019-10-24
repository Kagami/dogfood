CFLAGS = -D_POSIX_C_SOURCE=200809L -Wall -Wpedantic -Werror -g -std=c11
LDFLAGS = -no-pie
SRCS = $(wildcard *.c)
OBJS1 = $(addprefix .tmp1/,$(SRCS:.c=.o))
OBJS2 = $(addprefix .tmp2/,$(SRCS:.c=.o))
TSRCS = $(wildcard test/*.c)
TEST1 = $(addprefix .tmp1/,$(TSRCS:.c=))
TEST2 = $(addprefix .tmp2/,$(TSRCS:.c=))

all: stage1

stage1: $(OBJS1)
	$(CC) $(LDFLAGS) -o $@ $(OBJS1)

stage2: $(OBJS2)
	$(CC) $(LDFLAGS) -o $@ $(OBJS2)

test1: $(TEST1)
	@printf " \e[32mOK\e[0m\n"

test2: $(TEST2)
	@printf " \e[32mOK\e[0m\n"

fulltest: clean stage1 stage2 test1 test2

.tmp1/%.o: %.c dfcc.h
	@mkdir -p .tmp1
	$(CC) $(CFLAGS) -o $@ -c $<

.tmp2/%.o: %.c stage1
	@mkdir -p .tmp2
	./stage1 -I../libdfc/include -o .tmp2/$<.s $<
	$(CC) $(CFLAGS) -o $@ -c .tmp2/$<.s

.tmp1/test/%: test/%.c
	@mkdir -p .tmp1/test
	@$(CC) $(LDFLAGS) -o $@ $<
	@$@

.tmp2/test/%: test/%.c stage1
	@mkdir -p .tmp2/test
	@rm -f .tmp2/$<.o
	@./stage1 -I../libdfc/include -o .tmp2/$<.s $<
	@[ -f .tmp2/$<.o ] || $(CC) -o .tmp2/$<.o -c .tmp2/$<.s
	@$(CC) $(LDFLAGS) -o $@ .tmp2/$<.o
	@$@

clean:
	rm -rf stage1 stage2 .tmp1 .tmp2

.PHONY: $(TSRCS)
