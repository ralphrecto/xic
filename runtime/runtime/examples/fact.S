#include "defs.h"
	.text
	.globl	FUNC(Ifactorial)
	.align	4
FUNC(Ifactorial):
	pushq	%rbp
	movq	%rsp, %rbp
Ltmp2:
	subq	$32, %rsp
	movq	ARG1(), -16(%rbp)
	cmpq	$1, -16(%rbp)
	jg	LBB0_2

	movq	$1, -8(%rbp)
	jmp	LBB0_3
LBB0_2:
	movq	-16(%rbp), %rax
	movq	-16(%rbp), %rcx
	subq	$1, %rcx
	movq	%rcx, ARG1()
	movq	%rax, -24(%rbp)         ## 8-byte Spill
	callq	FUNC(Ifactorial)
	movq	-24(%rbp), %rcx         ## 8-byte Reload
	imulq	%rax, %rcx
	movq	%rcx, -8(%rbp)
LBB0_3:
	movq	-8(%rbp), %rax
	addq	$32, %rsp
	popq	%rbp
	retq

	.globl	FUNC(Imain_paai)
	.align	4
FUNC(Imain_paai):
	pushq	%rbp
Ltmp3:
Ltmp4:
	movq	%rsp, %rbp
Ltmp5:
	subq	$48, %rsp
	movq	ARG1(), -8(%rbp)
LBB1_1:
	callq	FUNC(Ieof_b)
	cmpq	$0, %rax
	setne	%cl
	xorb	$1, %cl
	testb	$1, %cl
	jne	LBB1_2
	jmp	LBB1_5
LBB1_2:
	leaq	_prompt(%rip), %rax
	addq	$8, %rax
	movq	%rax, ARG1()
	callq	FUNC(Iprint_pai)
	callq	FUNC(Ireadln_ai)
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), ARG1()
	callq	FUNC(IparseInt_t2ibai)
	movq	%rax, -40(%rbp)
	movq	-32(%rbp), %rax
	movq	%rdx, %rax
	movq	%rax, -32(%rbp)
	cmpq	$0, -32(%rbp)
	je	LBB1_4

	movq	-40(%rbp), ARG1()
	callq	FUNC(Ifactorial)
	movq	%rax, -48(%rbp)
	movq	-40(%rbp), ARG1()
	callq	FUNC(IunparseInt_aii)
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), ARG1()
	callq	FUNC(Iprint_pai)
	leaq	_is(%rip), %rax
	addq	$8, %rax
	movq	%rax, ARG1()
	callq	FUNC(Iprint_pai)
	movq	-48(%rbp), ARG1()
	callq	FUNC(IunparseInt_aii)
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), ARG1()
	callq	FUNC(Iprintln_pai)
LBB1_4:
	jmp	LBB1_1
LBB1_5:
	addq	$48, %rsp
	popq	%rbp
	retq

	.data
	.globl	_prompt
	.align	4
_prompt: ## string constant
	.quad	7                       
	.quad	78                      ## 0x4e N
	.quad	117                     ## 0x75 u
	.quad	109                     ## 0x6d m
	.quad	98                      ## 0x62 b
	.quad	101                     ## 0x65 e
	.quad	114                     ## 0x72 r
	.quad	63                      ## 0x3f ?

	.globl	_is
	.align	4
_is: ## string constant
	.quad	5                       
	.quad	33                      ## 0x21 !
	.quad	32                      ## 0x20
	.quad	105                     ## 0x69 i
	.quad	115                     ## 0x73 s
	.quad	32                      ## 0x20


