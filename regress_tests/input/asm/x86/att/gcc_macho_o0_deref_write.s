	.text
	.globl _y
	.zerofill __DATA,__pu_bss2,_y,4,2
	.globl _x
	.zerofill __DATA,__pu_bss2,_x,4,2
	.globl _t1r0
	.zerofill __DATA,__pu_bss2,_t1r0,4,2
	.globl _t0r0
	.zerofill __DATA,__pu_bss2,_t0r0,4,2
	.text
	.globl _P0
_P0:
LFB1:
	pushl	%ebp
LCFI0:
	movl	%esp, %ebp
LCFI1:
	subl	$16, %esp
	movl	$_y, -4(%ebp)
	movl	-4(%ebp), %eax
	movl	(%eax), %eax
	movl	%eax, -8(%ebp)
	movl	-8(%ebp), %eax
	movl	%eax, _t0r0
	movl	_t0r0, %eax
	cmpl	$1, %eax
	jne	L3
	movl	$1, _x
L3:
	nop
	leave
LCFI2:
	ret
LFE1:
	.globl _P1
_P1:
LFB2:
	pushl	%ebp
LCFI3:
	movl	%esp, %ebp
LCFI4:
	subl	$16, %esp
	movl	_x, %eax
	movl	%eax, _t1r0
	movl	_t1r0, %eax
	cmpl	$1, %eax
	jne	L6
	movl	$_y, -4(%ebp)
	movl	$1, -8(%ebp)
	movl	-8(%ebp), %eax
	movl	%eax, %edx
	movl	-4(%ebp), %eax
	movl	%edx, (%eax)
L6:
	nop
	leave
LCFI5:
	ret
LFE2:
	.section __TEXT,__eh_frame,coalesced,no_toc+strip_static_syms+live_support
EH_frame1:
	.set L$set$0,LECIE1-LSCIE1
	.long L$set$0
LSCIE1:
	.long	0
	.byte	0x1
	.ascii "zR\0"
	.byte	0x1
	.byte	0x7c
	.byte	0x8
	.byte	0x1
	.byte	0x10
	.byte	0xc
	.byte	0x5
	.byte	0x4
	.byte	0x88
	.byte	0x1
	.align 2
LECIE1:
LSFDE1:
	.set L$set$1,LEFDE1-LASFDE1
	.long L$set$1
LASFDE1:
	.long	LASFDE1-EH_frame1
	.long	LFB1-.
	.set L$set$2,LFE1-LFB1
	.long L$set$2
	.byte	0
	.byte	0x4
	.set L$set$3,LCFI0-LFB1
	.long L$set$3
	.byte	0xe
	.byte	0x8
	.byte	0x84
	.byte	0x2
	.byte	0x4
	.set L$set$4,LCFI1-LCFI0
	.long L$set$4
	.byte	0xd
	.byte	0x4
	.byte	0x4
	.set L$set$5,LCFI2-LCFI1
	.long L$set$5
	.byte	0xc4
	.byte	0xc
	.byte	0x5
	.byte	0x4
	.align 2
LEFDE1:
LSFDE3:
	.set L$set$6,LEFDE3-LASFDE3
	.long L$set$6
LASFDE3:
	.long	LASFDE3-EH_frame1
	.long	LFB2-.
	.set L$set$7,LFE2-LFB2
	.long L$set$7
	.byte	0
	.byte	0x4
	.set L$set$8,LCFI3-LFB2
	.long L$set$8
	.byte	0xe
	.byte	0x8
	.byte	0x84
	.byte	0x2
	.byte	0x4
	.set L$set$9,LCFI4-LCFI3
	.long L$set$9
	.byte	0xd
	.byte	0x4
	.byte	0x4
	.set L$set$10,LCFI5-LCFI4
	.long L$set$10
	.byte	0xc4
	.byte	0xc
	.byte	0x5
	.byte	0x4
	.align 2
LEFDE3:
	.subsections_via_symbols
