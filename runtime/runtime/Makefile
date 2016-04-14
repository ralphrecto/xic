CFLAGS=-g -Wall -O2 -m64 -fno-stack-protector
#CFLAGS=-g -Wall -O2 -fno-stack-protector
CC=gcc

# Boehm-Demers-Weiser collector
GCLIBDIR = gc-7.2
GCLIBZIP = gc-7.2f.tar.gz
GCLIB = $(GCLIBDIR)/.libs/libgc.a

GCLIBURL = http://hboehm.info/gc/gc_source/$(GCLIBZIP)

ABI_FLAG=$(shell ./platform-flags.sh)

all: xifilt libxi.a examples/arr examples/fact examples/fact-init

xifilt: demangle/demangle.o
	gcc $(ABI_FLAG) -o $@ $^

libxi.a: $(GCLIB) libxi/libxi.o
	rm -f $@
	cp $(GCLIB) $@
	ar rcs $@ libxi/libxi.o

%.o: %.c	
	gcc $(CFLAGS) $(ABI_FLAG) -c -o $@ $<

$(GCLIB): $(GCLIBDIR)/configure
	cd $(GCLIBDIR); ./configure; make; make check

$(GCLIBDIR)/configure:
	# Downloads and extracts Boehm GC
	rm -f $(GCLIB)
	rm -rf $(GCLIBDIR)
	wget -N $(GCLIBURL)
	tar -xzvf $(GCLIBZIP)
	if [ -f ../gcconfig.h ] ; \
	then cp ../gcconfig.h $(GCLIBDIR)/include/private/ ; \
	fi;

examples/fact:
	./linkxi.sh examples/fact.S -o examples/fact
	gcc -E examples/fact.S -o examples/fact.sx

examples/arr:
	./linkxi.sh examples/arr.S -o examples/arr
	gcc -E examples/arr.S -o examples/arr.sx

examples/fact-init:
	./linkxi.sh examples/init.S examples/fact.S -o examples/fact-init
	gcc -E examples/init.S -o examples/init.sx

clean:
	rm -f libxi.a xifilt
	rm -f libxi/libxi.o
	rm -f demangle/demangle.o
	rm -f examples/fact examples/arr examples/fact-init
	rm -f *~

clobber: clean
	$(MAKE) -C $(GCLIBDIR) clean
	cp ../gcconfig.h $(GCLIBDIR)/include/private/

dust: clobber
	rm -f $(GCLIBZIP)
	rm -rf $(GCLIBDIR)

FILES = runtime/demangle runtime/libxi \
        runtime/linkxi.sh runtime/Makefile runtime/README.txt runtime/$(GCLIBDIR)\
        runtime/examples/*.c runtime/examples/*.s runtime/include \
        runtime/ChangeLog.txt runtime/platform-flags.sh

tarballs: $(GCLIBDIR)/configure
	make clean
	cd .. && tar cvz --exclude=CVS --exclude="*~" ${FILES} > runtime.tar.gz
	cd .. && zip runtime.zip -r ${FILES} -x '*CVS*' -x "*~"
