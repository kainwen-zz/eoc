	.globl main
main:

	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp

	callq	read_int
	movq	%rax, -32(%rbp)
	callq	read_int
	movq	%rax, -8(%rbp)
	negq	-8(%rbp)
	movq	-8(%rbp), %rax
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	%rax, -24(%rbp)
	addq	$148, -24(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rax
	addq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax

	movq	%rax, %rdi
	callq	print_int

	addq	$48, %rsp
	popq	%rbp
	retq