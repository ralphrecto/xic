#include "defs.h"
    .text
    .globl  FUNC(Imain_paai)
    .align  4
FUNC(Imain_paai):
Lprologue:
    pushq   %rbp
    movq    %rsp, %rbp
Lbody:
    movq $1, %rcx
    movq $2, %rdx
    cmp  %rcx, %rdx
    # uncomment out this line and run make; it won't build
    # setne %rsi
Lepilogue:
    addq    $32, %rsp
    popq    %rbp
    retq

    .data
	.globl	Lstringa
	.align	4
Lstringa:
    .quad 1
    .quad 97
