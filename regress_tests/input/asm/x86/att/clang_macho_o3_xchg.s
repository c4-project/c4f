	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 14
	.globl	_P0                     ## -- Begin function P0
	.p2align	4, 0x90
_P0:                                    ## @P0
	.cfi_startproc
## %bb.0:
	pushl	%ebp
	.cfi_def_cfa_offset 8
	.cfi_offset %ebp, -8
	movl	%esp, %ebp
	.cfi_def_cfa_register %ebp
	movl	$2, %eax
	xchgl	%eax, _x
	movl	$1, %eax
	xchgl	%eax, _y
	popl	%ebp
	retl
	.cfi_endproc
                                        ## -- End function
	.globl	_P1                     ## -- Begin function P1
	.p2align	4, 0x90
_P1:                                    ## @P1
	.cfi_startproc
## %bb.0:
	pushl	%ebp
	.cfi_def_cfa_offset 8
	.cfi_offset %ebp, -8
	movl	%esp, %ebp
	.cfi_def_cfa_register %ebp
	movl	$2, %eax
	xchgl	%eax, _y
	movl	$1, %eax
	xchgl	%eax, _x
	popl	%ebp
	retl
	.cfi_endproc
                                        ## -- End function
	.globl	_y                      ## @y
.zerofill __DATA,__common,_y,4,2
	.globl	_x                      ## @x
.zerofill __DATA,__common,_x,4,2

.subsections_via_symbols
