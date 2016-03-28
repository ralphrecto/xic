# Compilation
CC=gcc
CFLAGS=-g -Wall -O2 -m64 -fno-stack-protector
ABI_FLAG=$(shell ./platform-flags.sh)

# Boehm-Demers-Weiser collector
GCLIBDIR = gc-7.2
GCLIBZIP = gc-7.2f.tar.gz
GCLIB = $(GCLIBDIR)/.libs/libgc.a
GCLIBURL = http://hboehm.info/gc/gc_source/$(GCLIBZIP)

# Examples to build
EXAMPLES := $(wildcard examples/*.S)
EXAMPLES := $(basename $(EXAMPLES))
EXAMPLES := $(filter-out examples/init,$(EXAMPLES))
EXAMPLES := $(EXAMPLES) examples/fact-init

all: xifilt libxi.a $(EXAMPLES)

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
	cp ../gcconfig.h $(GCLIBDIR)/include/private/

examples/%: examples/%.S
	./linkxi.sh $< -o $@
	gcc -E $< -o $@.sx

examples/fact-init:
	./linkxi.sh examples/init.S examples/fact.S -o examples/fact-init
	gcc -E examples/init.S -o examples/init.sx

clean:
	rm -f libxi.a xifilt
	rm -f libxi/libxi.o
	rm -f demangle/demangle.o
	rm -f examples/*.sx
	rm -f $(EXAMPLES)
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

print-%:
	@echo $*=$($*)
