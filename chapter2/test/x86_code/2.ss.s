	.globl main
main:

	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp

	callq	read_int
	movq	%rax, -16(%rbp)
	negq	-16(%rbp)
	movq	-16(%rbp), %rax
	movq	%rax, -24(%rbp)
	callq	read_int
	movq	%rax, -8(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -32(%rbp)
	movq	-8(%rbp), %rax
	addq	%rax, -32(%rbp)
	movq	$-53, -40(%rbp)
	movq	-32(%rbp), %rax
	addq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax

	movq	%rax, %rdi
	callq	print_int

	addq	$48, %rsp
	popq	%rbp
	retq
