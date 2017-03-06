	.globl main
main:

	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp

	callq	read_int
	movq	%rax, %r12
	negq	%r12
	movq	%r12, %r13
	callq	read_int
	movq	%rax, %r12
	addq	%r12, %r13
	movq	$-53, %r12
	addq	%r13, %r12
	movq	%r12, %rax

	movq	%rax, %rdi
	callq	print_int

	addq	$48, %rsp
	popq	%rbp
	retq
