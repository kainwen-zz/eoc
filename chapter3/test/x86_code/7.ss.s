	.globl main
main:

	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp

	movq	$4, -24(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -16(%rbp)
	addq	$1, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	%rax, -8(%rbp)
	addq	$2, -8(%rbp)
	movq	-8(%rbp), %rax

	movq	%rax, %rdi
	callq	print_int

	addq	$32, %rsp
	popq	%rbp
	retq
