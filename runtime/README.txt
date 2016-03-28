==========================
= CS4120 Runtime Library =
==========================

What's under the runtime/ directory?
=============
include/ --- the Xi library interface files.

libxi/ --- the source code for the runtime library you'll want to link into
programs your compiler produces. This has the implementations of
things like _I_alloc_i, and the various methods given in the
io and conv modules.

demangle/ --- a little utility that can turn things like _Imain_paii
into things like main(int[][]). You can pass error messages from the
linker through it to make them nicer, or use it to help debug
name encoding problems.

This gets compiled into xifilt.

examples/ --- the .s files are of most interest here, since they show
what your output might look like, and give examples of syntax
to produce. With libxi compiled, you can turn them into runnable form
by doing something like this:
    gcc -o arr example/arr.s -L. -lxi -lpthread

The -L. option tells to look for libraries in the current directory (.)
and -lxi tells to link in libxi. -o arr tells to name the output
arr (or arr.exe). If you don't include the -o, it'll get named
something like a.out or a.exe. GCC is used for this since it knows how to find
the assembler and the linker and what standard libraries to include.
Note that the C compiler (cc1) isn't actually invoked here.

You can use a command like this to test your output as well. For
convenience, we also include a little linker script, linkxi.sh. It
links your code the same way, but also passes the output through
xifilt to get nicer link errors, and tries to figure out where libxi
is located so it can work if you invoke it from a different directory.
The makefile uses it to compile examples, like this:

    ./linkxi.sh examples/fact.s -o examples/fact

The .s files were not produced by hand. They were created by running
    gcc -S -O0 -fno-stack-protector -I../libxi

on corresponding .c files, then hand-eliminating platform-dependent
contents, and adding the comments. To get ideas for instruction selection,
you might want to use the same command to compile your own C code.

Makefile --- tells the make command how to compile all of the above.

Building
========

The Makefile will download and build the Boehm-Demers-Weiser
conservative garbage collector. The runtime library relies on it for GC.
See http://hboehm.info/gc/ for more information.

The Xi runtime library and the garbage collector library are combined by
the Makefile into a single library: libxi.a

Linux VM:
-------------------------
This runtime has been tested to work on the released Vagrant virtual machine.

Windows:
-------------------------
To use this runtime, and to test your compiler on Windows, you'll need to
install Cygwin and the gcc toolchain for it. (Cygwin is a compatibility
layer that lets one use Unix programs like gcc, ld, as and make on Windows)

To do this, do the following:
1) Go to cygwin.org
2) Click the "Install cygwin now" link to download its setup.exe
3) Run setup.exe
4) Go through the wizard. When it asks what packages to install,
   under the Devel category, make sure to activate "gcc" and "make"
5) When finished, you'll get a start menu/desktop icon that launches the
   cygwin interface. Now you can go where you have this archive extracted,
   and type make.

In case you're not familiar with Unix command-line, you may find the following
helpful:
- Paths are separate with forward slashes, not backwards slashes
- Cygwin turns drive letters into subdirectories of /cygdrive/. So for
  example E:\CS4120\PA5 will become /cygdrive/e/CS4120/PA5
- Important commands:
    cd --- change directory. cd .. moves up one level
    ls --- list current directory
    pwd --- print working directory

OS X:
----------------------
You will need gcc. It's included in the Xcode tools package.

If you're using Snow Leopard, you'll need to add -m64 to ABI_FLAG settings
in Makefile and linkxi.sh

When it's installed, go to directory where this archive is extracted, and run
make. (See the end of the above section if you're unfamiliar with the
command line)

Note: OS X's assembler seems to have problems with Intel syntax.

Unix:
-------------------
Make sure you have gcc installed, and run make (or gmake, on BSD systems)
in this archives root directory
