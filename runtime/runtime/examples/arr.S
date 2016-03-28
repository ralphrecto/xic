#include "defs.h"
    .text
    .globl  FUNC(Imain_paai)
    .align  4
FUNC(Imain_paai):

    pushq   %rbp
Ltmp0:
Ltmp1:
    movq    %rsp, %rbp
Ltmp2:
    subq    $32, %rsp
    movq    ARG1(), -8(%rbp)
    movq    -8(%rbp), %rdi
    movq    -8(%rdi), %rdi
    movq    %rdi, -16(%rbp)
    movq    $0, -24(%rbp)
LBB0_1:                                 ## =>This Inner Loop Header: Depth=1
    movq    -24(%rbp), %rax
    cmpq    -16(%rbp), %rax
    jge LBB0_4
## BB#2:                                ##   in Loop: Header=BB0_1 Depth=1
    movq    -24(%rbp), %rax
    movq    -8(%rbp), %rcx
    movq    (%rcx,%rax,8), ARG1()
    callq   FUNC(Iprintln_pai)
## BB#3:                                ##   in Loop: Header=BB0_1 Depth=1
    movq    -24(%rbp), %rax
    addq    $1, %rax
    movq    %rax, -24(%rbp)
    jmp LBB0_1
LBB0_4:
    addq    $32, %rsp
    popq    %rbp
    retq

