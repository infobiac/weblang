	.text
	.file	"examples/slack.ll"
	.globl	sendMsg
	.p2align	4, 0x90
	.type	sendMsg,@function
sendMsg:                                # @sendMsg
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$200, %rsp
.Lcfi0:
	.cfi_def_cfa_offset 208
	movq	%rdi, 192(%rsp)
	movb	$81, 78(%rsp)
	movb	$97, 77(%rsp)
	movb	$66, 76(%rsp)
	movb	$47, 75(%rsp)
	movb	$78, 74(%rsp)
	movb	$78, 73(%rsp)
	movb	$89, 72(%rsp)
	movb	$53, 71(%rsp)
	movb	$88, 70(%rsp)
	movb	$49, 69(%rsp)
	movb	$57, 68(%rsp)
	movb	$56, 67(%rsp)
	movb	$66, 66(%rsp)
	movb	$47, 65(%rsp)
	movb	$78, 64(%rsp)
	movb	$48, 63(%rsp)
	movb	$74, 62(%rsp)
	movb	$55, 61(%rsp)
	movb	$87, 60(%rsp)
	movb	$82, 59(%rsp)
	movb	$52, 58(%rsp)
	movb	$55, 57(%rsp)
	movb	$84, 56(%rsp)
	movb	$47, 55(%rsp)
	movb	$115, 54(%rsp)
	movb	$101, 53(%rsp)
	movb	$99, 52(%rsp)
	movb	$105, 51(%rsp)
	movb	$118, 50(%rsp)
	movb	$114, 49(%rsp)
	movb	$101, 48(%rsp)
	movb	$115, 47(%rsp)
	movb	$47, 46(%rsp)
	movb	$109, 45(%rsp)
	movb	$111, 44(%rsp)
	movb	$99, 43(%rsp)
	movb	$46, 42(%rsp)
	movb	$107, 41(%rsp)
	movb	$99, 40(%rsp)
	movb	$97, 39(%rsp)
	movb	$108, 38(%rsp)
	movb	$115, 37(%rsp)
	movb	$46, 36(%rsp)
	movb	$115, 35(%rsp)
	movb	$107, 34(%rsp)
	movb	$111, 33(%rsp)
	movb	$111, 32(%rsp)
	movb	$104, 31(%rsp)
	movb	$47, 30(%rsp)
	movb	$47, 29(%rsp)
	movb	$58, 28(%rsp)
	movb	$115, 27(%rsp)
	movb	$112, 26(%rsp)
	movb	$116, 25(%rsp)
	movb	$116, 24(%rsp)
	movb	$104, 23(%rsp)
	movb	$34, 22(%rsp)
	movb	$58, 21(%rsp)
	movb	$34, 20(%rsp)
	movb	$108, 19(%rsp)
	movb	$114, 18(%rsp)
	movb	$117, 17(%rsp)
	movb	$34, 16(%rsp)
	movb	$123, 15(%rsp)
	movb	$101, 142(%rsp)
	movb	$114, 141(%rsp)
	movb	$97, 140(%rsp)
	movb	$32, 139(%rsp)
	movb	$119, 138(%rsp)
	movb	$111, 137(%rsp)
	movb	$104, 136(%rsp)
	movb	$32, 135(%rsp)
	movb	$44, 134(%rsp)
	movb	$101, 133(%rsp)
	movb	$105, 132(%rsp)
	movb	$122, 131(%rsp)
	movb	$122, 130(%rsp)
	movb	$105, 129(%rsp)
	movb	$76, 128(%rsp)
	movb	$32, 127(%rsp)
	movb	$121, 126(%rsp)
	movb	$101, 125(%rsp)
	movb	$72, 124(%rsp)
	movb	$39, 123(%rsp)
	movb	$58, 122(%rsp)
	movb	$39, 121(%rsp)
	movb	$116, 120(%rsp)
	movb	$120, 119(%rsp)
	movb	$101, 118(%rsp)
	movb	$116, 117(%rsp)
	movb	$39, 116(%rsp)
	movb	$123, 115(%rsp)
	movb	$34, 114(%rsp)
	movb	$32, 113(%rsp)
	movb	$58, 112(%rsp)
	movb	$34, 111(%rsp)
	movb	$100, 110(%rsp)
	movb	$97, 109(%rsp)
	movb	$111, 108(%rsp)
	movb	$108, 107(%rsp)
	movb	$121, 106(%rsp)
	movb	$97, 105(%rsp)
	movb	$112, 104(%rsp)
	movb	$34, 103(%rsp)
	movb	$32, 102(%rsp)
	movb	$44, 101(%rsp)
	movb	$34, 100(%rsp)
	movb	$49, 99(%rsp)
	movb	$82, 98(%rsp)
	movb	$114, 97(%rsp)
	movb	$80, 96(%rsp)
	movb	$54, 95(%rsp)
	movb	$69, 94(%rsp)
	movb	$69, 93(%rsp)
	movb	$51, 92(%rsp)
	movb	$72, 91(%rsp)
	movb	$72, 90(%rsp)
	movb	$75, 89(%rsp)
	movb	$78, 88(%rsp)
	movb	$81, 87(%rsp)
	movb	$81, 86(%rsp)
	movb	$109, 85(%rsp)
	movb	$84, 84(%rsp)
	movb	$76, 83(%rsp)
	movb	$108, 82(%rsp)
	movb	$102, 81(%rsp)
	movb	$108, 80(%rsp)
	movb	$72, 79(%rsp)
	movb	$0, 183(%rsp)
	movb	$125, 182(%rsp)
	movb	$34, 181(%rsp)
	movb	$125, 180(%rsp)
	movb	$39, 179(%rsp)
	movb	$58, 178(%rsp)
	movb	$115, 177(%rsp)
	movb	$100, 176(%rsp)
	movb	$114, 175(%rsp)
	movb	$97, 174(%rsp)
	movb	$119, 173(%rsp)
	movb	$100, 172(%rsp)
	movb	$101, 171(%rsp)
	movb	$58, 170(%rsp)
	movb	$32, 169(%rsp)
	movb	$105, 168(%rsp)
	movb	$106, 167(%rsp)
	movb	$111, 166(%rsp)
	movb	$109, 165(%rsp)
	movb	$101, 164(%rsp)
	movb	$32, 163(%rsp)
	movb	$115, 162(%rsp)
	movb	$105, 161(%rsp)
	movb	$104, 160(%rsp)
	movb	$116, 159(%rsp)
	movb	$32, 158(%rsp)
	movb	$116, 157(%rsp)
	movb	$117, 156(%rsp)
	movb	$111, 155(%rsp)
	movb	$32, 154(%rsp)
	movb	$107, 153(%rsp)
	movb	$99, 152(%rsp)
	movb	$101, 151(%rsp)
	movb	$104, 150(%rsp)
	movb	$67, 149(%rsp)
	movb	$32, 148(%rsp)
	movb	$63, 147(%rsp)
	movb	$117, 146(%rsp)
	movb	$111, 145(%rsp)
	movb	$121, 144(%rsp)
	movb	$32, 143(%rsp)
	leaq	15(%rsp), %rdi
	callq	json_string
	movq	%rax, 184(%rsp)
	movq	%rax, %rdi
	callq	executeMsg
	addq	$200, %rsp
	retq
.Lfunc_end0:
	.size	sendMsg, .Lfunc_end0-sendMsg
	.cfi_endproc

	.globl	executeMsg
	.p2align	4, 0x90
	.type	executeMsg,@function
executeMsg:                             # @executeMsg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Lcfi1:
	.cfi_def_cfa_offset 16
	subq	$32, %rsp
.Lcfi2:
	.cfi_def_cfa_offset 48
.Lcfi3:
	.cfi_offset %rbx, -16
	movq	%rdi, 24(%rsp)
	callq	tostring
	movq	%rax, %rdi
	callq	post
	movb	$0, 23(%rsp)
	movb	$111, 22(%rsp)
	movb	$110, 21(%rsp)
	movb	$105, 20(%rsp)
	movb	$110, 19(%rsp)
	movb	$32, 18(%rsp)
	movb	$110, 17(%rsp)
	movb	$101, 16(%rsp)
	movb	$117, 15(%rsp)
	movb	$98, 14(%rsp)
	movb	$32, 13(%rsp)
	movb	$110, 12(%rsp)
	movb	$117, 11(%rsp)
	movb	$32, 10(%rsp)
	movb	$115, 9(%rsp)
	movb	$101, 8(%rsp)
	movb	$32, 7(%rsp)
	movb	$110, 6(%rsp)
	movb	$97, 5(%rsp)
	movb	$100, 4(%rsp)
	movb	$114, 3(%rsp)
	movb	$111, 2(%rsp)
	movb	$106, 1(%rsp)
	leaq	1(%rsp), %rdi
	callq	json_string
	movq	%rax, %rbx
	movq	%rbx, %rdi
	callq	tostring
	movq	%rax, %rdi
	callq	puts
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	retq
.Lfunc_end1:
	.size	executeMsg, .Lfunc_end1-executeMsg
	.cfi_endproc

	.globl	main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Lcfi4:
	.cfi_def_cfa_offset 16
.Lcfi5:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Lcfi6:
	.cfi_def_cfa_register %rbp
	pushq	%r14
	pushq	%rbx
	subq	$16, %rsp
.Lcfi7:
	.cfi_offset %rbx, -32
.Lcfi8:
	.cfi_offset %r14, -24
	movq	8(%rsi), %rbx
	movq	16(%rsi), %r14
	movb	$0, -17(%rbp)
	movb	$103, -18(%rbp)
	movb	$115, -19(%rbp)
	movb	$77, -20(%rbp)
	movb	$100, -21(%rbp)
	movb	$110, -22(%rbp)
	movb	$101, -23(%rbp)
	movb	$115, -24(%rbp)
	leaq	-24(%rbp), %rsi
	movq	%rbx, %rdi
	callq	strcmp
	testl	%eax, %eax
	jne	.LBB2_2
# BB#1:                                 # %sendMsg
	movq	%r14, %rdi
	callq	json_string
	movq	%rax, %rdi
	callq	sendMsg
.LBB2_2:                                # %continue
	movq	%rsp, %rax
	leaq	-16(%rax), %rsi
	movq	%rsi, %rsp
	movb	$0, -6(%rax)
	movb	$103, -7(%rax)
	movb	$115, -8(%rax)
	movb	$77, -9(%rax)
	movb	$101, -10(%rax)
	movb	$116, -11(%rax)
	movb	$117, -12(%rax)
	movb	$99, -13(%rax)
	movb	$101, -14(%rax)
	movb	$120, -15(%rax)
	movb	$101, -16(%rax)
	movq	%rbx, %rdi
	callq	strcmp
	testl	%eax, %eax
	jne	.LBB2_4
# BB#3:                                 # %executeMsg
	movq	%r14, %rdi
	callq	json_string
	movq	%rax, %rdi
	callq	executeMsg
.LBB2_4:                                # %continue1
	xorl	%eax, %eax
	leaq	-16(%rbp), %rsp
	popq	%rbx
	popq	%r14
	popq	%rbp
	retq
.Lfunc_end2:
	.size	main, .Lfunc_end2-main
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
