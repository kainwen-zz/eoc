	.globl main
main:

	pushq	%rbp
	movq	%rsp, %rbp
	subq	$80, %rsp

	callq	read_int
	movq	%rax, %r15
	callq	read_int
	movq	%rax, %r14
	callq	read_int
	movq	%rax, %r12
	negq	%r12
	movq	%r12, %r13
	callq	read_int
	movq	%rax, %r12
	addq	%r12, %r13
	movq	%r14, %r12
	addq	%r13, %r12
	movq	%r15, %r13
	addq	%r12, %r13
	movq	$-24, %r12
	addq	%r13, %r12
	movq	%r12, %rax

	movq	%rax, %rdi
	callq	print_int

	addq	$80, %rsp
	popq	%rbp
	retq
