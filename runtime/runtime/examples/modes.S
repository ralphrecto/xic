#include "defs.h"
    .text
    .globl  FUNC(Imain_paai)
    .align  4
FUNC(Imain_paai):
Lprologue:
    pushq   %rbp
    movq    %rsp, %rbp
Lbody:
    subq    $32, %rsp
    leaq    Lstringa(%rip), %rax
    addq    $8, %rax
    movq    %rax, -8(%rbp)

    ## (%r)
    leaq    -8(%rbp), %rax
    movq    (%rax), ARG1()
    callq   FUNC(Iprintln_pai)

    ## (,%r,{,1,2,4,8})
    leaq    -8(%rbp), %rax
    movq    (,%rax,1), ARG1()
    callq   FUNC(Iprintln_pai)

    ## (%r,%r,{1,2,4,8})
    movq    $-8, %rax
    movq    (%rbp,%rax,1), ARG1()
    callq   FUNC(Iprintln_pai)

    ## n(%r)
    movq    -8(%rbp), ARG1()
    callq   FUNC(Iprintln_pai)

    ## n(,%r,{1,2,4,8})
    movq    -8(,%rbp,1), ARG1()
    callq   FUNC(Iprintln_pai)

    ## n(%r,%r,{1,2,4,8})
    movq    $-16, %rax
    movq    8(%rbp,%rax,1), ARG1()
    callq   FUNC(Iprintln_pai)
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
