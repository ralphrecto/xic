#include "defs.h"
	# An example of initialization.
	# link it in with some other XI program and it will do an
	# extra printf before the program starts.
	.data
	.align 8
LC1:
	.ascii "Initializer called\n\0"
	.align 8
LC2:
	.quad 3

	.align 8
    .lcomm LC3, 64

	.text
test1:
	pushq	%rbp
    movq    $7, LC2(%rip)
    movq    LC2(%rip), %rax
	movq	%rsp, %rbp
	leaq    LC1(%rip), ARG1()
    movb    $89, (ARG1(), %rax, 1)
    movq    LC3(%rip), %rax
    movb    $89, (ARG1(), %rax, 1)

	callq	GCFUNC(GC_printf)
	leave
	retq

# table of functions to call automatically
	INITFUNC()
	.align 4
	.quad	test1
