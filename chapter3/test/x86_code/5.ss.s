	.globl main
main:

	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp

	callq	read_int
	movq	%rax, %r12
	movq	$64, %r13
	addq	%r12, %r13
	movq	%r13, %rax

	movq	%rax, %rdi
	callq	print_int

	addq	$16, %rsp
	popq	%rbp
	retq
