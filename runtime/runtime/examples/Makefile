.DEFAULT_GOAL = default

ASM = fact.s arr.s
EXECS = fact arr
LIBXI = ../libxi/libxi.o
GCLIBDIR = ../gc-7.2/.libs/

default: $(ASM) $(EXECS)

CC = gcc
ABI_FLAG = $(shell ../platform-flags.sh)
CINCS = -I../libxi
CFLAGS = -O0 -fno-stack-protector $(ABI_FLAG) $(CINCS)

.SUFFIXES: .s

%.s: %.c
	$(CC) $(CFLAGS) -S $<
	perl -ni -e 'if (!/InlineAsm/) { print $$_ }' $*.s

%.o: %.c
	$(CC) $(CFLAGS) -c $<

fact: fact.o
	$(CC) -o fact fact.o $(LIBXI) -L$(GCLIBDIR) -lgc

arr: arr.o
	$(CC) -o arr arr.o $(LIBXI) -L$(GCLIBDIR) -lgc

clean:
	-rm -f $(DIRT)


DIRT = *.o $(ASM) $(EXECS)
