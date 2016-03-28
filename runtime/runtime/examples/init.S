#include "defs.h"
	# An example of initialization.
	# link it in with some other XI program and it will do an
	# extra printf before the program starts.
	.data
	.align 8
LC1:
	.ascii "Initializer called\n\0"

	.text
test1:
	pushq	%rbp
	movq	%rsp, %rbp
	leaq    LC1(%rip), ARG1()

	callq	GCFUNC(GC_printf)
	leave
	retq

# table of functions to call automatically
	INITFUNC()
	.align 4
	.quad	test1
