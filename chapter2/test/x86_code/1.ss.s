	.globl main
main:

	pushq	%rbp
	movq	%rsp, %rbp
	subq	$128, %rsp

	callq	read_int
	movq	%rax, -120(%rbp)
	negq	-120(%rbp)
	movq	-120(%rbp), %rax
	movq	%rax, -128(%rbp)
	callq	read_int
	movq	%rax, -104(%rbp)
	negq	-104(%rbp)
	movq	-104(%rbp), %rax
	movq	%rax, -112(%rbp)
	movq	-128(%rbp), %rax
	movq	%rax, -96(%rbp)
	movq	-112(%rbp), %rax
	addq	%rax, -96(%rbp)
	negq	-96(%rbp)
	movq	-96(%rbp), %rax
	movq	%rax, -88(%rbp)
	movq	$235, -80(%rbp)
	negq	-80(%rbp)
	movq	-80(%rbp), %rax
	movq	%rax, -64(%rbp)
	movq	-64(%rbp), %rax
	movq	%rax, -72(%rbp)
	addq	$-35, -72(%rbp)
	movq	-88(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-72(%rbp), %rax
	addq	%rax, -56(%rbp)
	callq	read_int
	movq	%rax, -24(%rbp)
	negq	-24(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -32(%rbp)
	callq	read_int
	movq	%rax, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	%rax, -16(%rbp)
	addq	$246, -16(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -40(%rbp)
	movq	-16(%rbp), %rax
	addq	%rax, -40(%rbp)
	movq	-56(%rbp), %rax
	movq	%rax, -48(%rbp)
	movq	-40(%rbp), %rax
	addq	%rax, -48(%rbp)
	movq	-48(%rbp), %rax

	movq	%rax, %rdi
	callq	print_int

	addq	$128, %rsp
	popq	%rbp
	retq
