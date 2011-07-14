
	.globl	___stginit_Main         ## @__stginit_Main
.zerofill __DATA,__common,___stginit_Main,1,3
	.section	__DATA,__data
	.globl	_Main_main3_closure     ## @Main_main3_closure
	.align	3
_Main_main3_closure:
	.quad	_integerzmgmp_GHCziIntegerziType_Szh_static_info
	.quad	1                       ## 0x1
	.section	__DATA,__const
	.globl	_Main_main2_srt         ## @Main_main2_srt
	.align	3
_Main_main2_srt:
	.quad	_base_GHCziFloat_zdwzdcfromRational_closure
	.section	__DATA,__data
	.globl	_Main_main2_closure     ## @Main_main2_closure
	.align	4
_Main_main2_closure:
	.quad	_Main_main2_info
	.quad	0                       ## 0x0
	.quad	0                       ## 0x0
	.quad	0                       ## 0x0
	.section	__DATA,__const
	.globl	_Main_main1_srt         ## @Main_main1_srt
	.align	4
_Main_main1_srt:
	.quad	_base_GHCziFloat_zdfShowDouble1_closure
	.quad	_base_GHCziIOziHandleziFD_stdout_closure
	.quad	_base_GHCziIOziHandleziText_hPutStr2_closure
	.quad	_Main_main2_closure
	.section	__DATA,__data
	.globl	_Main_main1_closure     ## @Main_main1_closure
	.align	3
_Main_main1_closure:
	.quad	_Main_main1_info
	.quad	0                       ## 0x0
	.section	__DATA,__const
	.globl	_Main_main4_srt         ## @Main_main4_srt
	.align	3
_Main_main4_srt:
	.quad	_base_GHCziTopHandler_runMainIO1_closure
	.quad	_Main_main1_closure
	.section	__DATA,__data
	.globl	_Main_main4_closure     ## @Main_main4_closure
	.align	3
_Main_main4_closure:
	.quad	_Main_main4_info
	.quad	0                       ## 0x0
	.section	__DATA,__const
	.globl	_Main_main_srt          ## @Main_main_srt
	.align	3
_Main_main_srt:
	.quad	_Main_main1_closure
	.section	__DATA,__data
	.globl	_Main_main_closure      ## @Main_main_closure
	.align	3
_Main_main_closure:
	.quad	_Main_main_info
	.quad	0                       ## 0x0
	.section	__DATA,__const
	.globl	_ZCMain_main_srt        ## @ZCMain_main_srt
	.align	3
_ZCMain_main_srt:
	.quad	_Main_main4_closure
	.section	__DATA,__data
	.globl	_ZCMain_main_closure    ## @ZCMain_main_closure
	.align	3
_ZCMain_main_closure:
	.quad	_ZCMain_main_info
	.quad	0                       ## 0x0

	.text
	.globl	_Main_main2_info_itable ## @Main_main2_info_itable
	.align	3
_Main_main2_info_itable:
	.quad	_Main_main2_srt-_Main_main2_info
	.quad	0                       ## 0x0
	.quad	4294967318              ## 0x100000016
	.section	__TEXT,__text,regular,pure_instructions

	.text
	.globl	_Main_main2_info
	.align	3, 0x90
_Main_main2_info:                       ## @Main_main2_info
## BB#0:                                ## %c1C3
	subq	$24, %rsp
	leaq	-16(%rbp), %rax
	movq	%rax, 16(%rsp)          ## 8-byte Spill
	cmpq	%r15, %rax
	jb	LBB0_3
## BB#1:                                ## %n1Cb
	leaq	16(%r12), %rax
	movq	%rax, 8(%rsp)           ## 8-byte Spill
	cmpq	144(%r13), %rax
	jbe	LBB0_4
## BB#2:                                ## %c1Co
	movq	$16, 184(%r13)
	movq	%rax, %r12
LBB0_3:                                 ## %c1Ca
	movq	-16(%r13), %rax
	addq	$24, %rsp
	jmpq	*%rax  # TAILCALL
LBB0_4:                                 ## %n1Cp
	movq	_stg_CAF_BLACKHOLE_info@GOTPCREL(%rip), %rax
	movq	%rax, 8(%r12)
	movq	152(%r13), %rax
	movq	%rax, 16(%r12)
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	_newCAF
	addq	$8, %r12
	movq	%r12, 8(%rbx)
	movq	_stg_IND_STATIC_info@GOTPCREL(%rip), %rax
	movq	%rax, (%rbx)
	movq	_stg_bh_upd_frame_info@GOTPCREL(%rip), %rax
	movq	%rax, -16(%rbp)
	movq	%r12, -8(%rbp)
	leaq	_Main_main3_closure+1(%rip), %r14
	movq	16(%rsp), %rbp          ## 8-byte Reload
	movq	8(%rsp), %r12           ## 8-byte Reload
	movq	%r14, %rsi
	addq	$24, %rsp
	jmp	_base_GHCziFloat_zdwzdcfromRational_info ## TAILCALL

	.text
	.align	3                       ## @s1zP_info_itable
_s1zP_info_itable:
	.quad	_Main_main1_srt-_s1zP_info
	.quad	4294967296              ## 0x100000000
	.quad	4294967314              ## 0x100000012

	.text
	.align	3, 0x90
_s1zP_info:                             ## @s1zP_info
## BB#0:                                ## %c1EY
	leaq	-32(%rbp), %rax
	cmpq	%r15, %rax
	jb	LBB1_2
## BB#1:                                ## %n1F6
	movq	_stg_upd_frame_info@GOTPCREL(%rip), %rcx
	movq	%rcx, -16(%rbp)
	movq	%rbx, -8(%rbp)
	movq	_ghczmprim_GHCziTypes_ZMZN_closure@GOTPCREL(%rip), %rcx
	incq	%rcx
	movq	%rcx, -24(%rbp)
	movq	_stg_ap_p_info@GOTPCREL(%rip), %rcx
	movq	%rcx, -32(%rbp)
	movq	_base_GHCziFloat_zdfShowDouble1_closure@GOTPCREL(%rip), %r14
	incq	%r14
	movsd	16(%rbx), %xmm5
	movq	_base_GHCziBase_zzeroInt_closure@GOTPCREL(%rip), %rsi
	movq	%rax, %rbp
	jmp	_base_GHCziFloat_zdwzdsshowSignedFloat_info ## TAILCALL
LBB1_2:                                 ## %c1F5
	movq	-16(%r13), %rax
	jmpq	*%rax  # TAILCALL

	.text
	.align	3                       ## @s1zQ_info_itable
_s1zQ_info_itable:
	.quad	_Main_main1_srt-_s1zQ_info
	.quad	0                       ## 0x0
	.quad	30064771104             ## 0x700000020

	.text
	.align	3, 0x90
_s1zQ_info:                             ## @s1zQ_info
## BB#0:                                ## %c1Hk
	subq	$8, %rsp
	leaq	24(%r12), %rax
	movq	%rax, (%rsp)            ## 8-byte Spill
	cmpq	144(%r13), %rax
	ja	LBB2_2
## BB#1:                                ## %n1Hz
	movsd	7(%rbx), %xmm0
	callq	_sin
	leaq	_s1zP_info(%rip), %rax
	movq	%rax, 8(%r12)
	movsd	%xmm0, 24(%r12)
	addq	$8, %rbp
	movq	_ghczmprim_GHCziTypes_True_closure@GOTPCREL(%rip), %rdi
	addq	$2, %rdi
	movq	%r12, %rsi
	addq	$8, %rsi
	movq	_base_GHCziIOziHandleziFD_stdout_closure@GOTPCREL(%rip), %r14
	movq	(%rsp), %r12            ## 8-byte Reload
	addq	$8, %rsp
	jmp	_base_GHCziIOziHandleziText_hPutStr2_info ## TAILCALL
LBB2_2:                                 ## %c1Hy
	movq	$24, 184(%r13)
	movq	-16(%r13), %rax
	movq	(%rsp), %r12            ## 8-byte Reload
	addq	$8, %rsp
	jmpq	*%rax  # TAILCALL

	.text
	.globl	_Main_main1_info_itable ## @Main_main1_info_itable
	.align	3
_Main_main1_info_itable:
	.quad	_Main_main1_srt-_Main_main1_info
	.quad	4294967299              ## 0x100000003
	.quad	0                       ## 0x0
	.quad	64424509455             ## 0xf0000000f

	.text
	.globl	_Main_main1_info
	.align	3, 0x90
_Main_main1_info:                       ## @Main_main1_info
## BB#0:                                ## %c1JQ
	subq	$8, %rsp
	leaq	-8(%rbp), %rax
	cmpq	%r15, %rax
	jb	LBB3_3
## BB#1:                                ## %n1JY
	leaq	_s1zQ_info(%rip), %rcx
	movq	%rcx, (%rax)
	leaq	_Main_main2_closure(%rip), %rcx
	testb	$7, %cl
	jne	LBB3_4
## BB#2:                                ## %n1Kd
	movq	_Main_main2_closure(%rip), %rcx
	leaq	_Main_main2_closure(%rip), %rbx
	movq	%rax, %rbp
	addq	$8, %rsp
	jmpq	*%rcx  # TAILCALL
LBB3_3:                                 ## %c1JX
	movq	-8(%r13), %rax
	leaq	_Main_main1_closure(%rip), %rbx
	addq	$8, %rsp
	jmpq	*%rax  # TAILCALL
LBB3_4:                                 ## %c1Kc
	leaq	24(%r12), %rcx
	movq	%rcx, (%rsp)            ## 8-byte Spill
	cmpq	144(%r13), %rcx
	ja	LBB3_6
## BB#5:                                ## %n1Hz.i
	movsd	_Main_main2_closure+7(%rip), %xmm0
	callq	_sin
	leaq	_s1zP_info(%rip), %rax
	movq	%rax, 8(%r12)
	movsd	%xmm0, 24(%r12)
	movq	_ghczmprim_GHCziTypes_True_closure@GOTPCREL(%rip), %rdi
	addq	$2, %rdi
	movq	%r12, %rsi
	addq	$8, %rsi
	leaq	_Main_main2_closure(%rip), %rbx
	movq	_base_GHCziIOziHandleziFD_stdout_closure@GOTPCREL(%rip), %r14
	movq	(%rsp), %r12            ## 8-byte Reload
	addq	$8, %rsp
	jmp	_base_GHCziIOziHandleziText_hPutStr2_info ## TAILCALL
LBB3_6:                                 ## %c1Hy.i
	movq	$24, 184(%r13)
	movq	-16(%r13), %rcx
	leaq	_Main_main2_closure(%rip), %rbx
	movq	%rax, %rbp
	movq	(%rsp), %r12            ## 8-byte Reload
	addq	$8, %rsp
	jmpq	*%rcx  # TAILCALL

	.text
	.globl	_Main_main4_info_itable ## @Main_main4_info_itable
	.align	3
_Main_main4_info_itable:
	.quad	_Main_main4_srt-_Main_main4_info
	.quad	4294967299              ## 0x100000003
	.quad	0                       ## 0x0
	.quad	12884901903             ## 0x30000000f

	.text
	.globl	_Main_main4_info
	.align	3, 0x90
_Main_main4_info:                       ## @Main_main4_info
## BB#0:                                ## %c1Lw
	leaq	_Main_main1_closure+1(%rip), %r14
	jmp	_base_GHCziTopHandler_runMainIO1_info ## TAILCALL

	.text
	.globl	_Main_main_info_itable  ## @Main_main_info_itable
	.align	3
_Main_main_info_itable:
	.quad	_Main_main_srt-_Main_main_info
	.quad	4294967299              ## 0x100000003
	.quad	0                       ## 0x0
	.quad	4294967311              ## 0x10000000f

	.text
	.globl	_Main_main_info
	.align	3, 0x90
_Main_main_info:                        ## @Main_main_info
## BB#0:                                ## %c1M9
	jmp	_Main_main1_info        ## TAILCALL

	.text
	.globl	_ZCMain_main_info_itable ## @ZCMain_main_info_itable
	.align	3
_ZCMain_main_info_itable:
	.quad	_ZCMain_main_srt-_ZCMain_main_info
	.quad	4294967299              ## 0x100000003
	.quad	0                       ## 0x0
	.quad	4294967311              ## 0x10000000f

	.text
	.globl	_ZCMain_main_info
	.align	3, 0x90
_ZCMain_main_info:                      ## @ZCMain_main_info
## BB#0:                                ## %c1MK
	leaq	_Main_main1_closure+1(%rip), %r14
	jmp	_base_GHCziTopHandler_runMainIO1_info ## TAILCALL
	.no_dead_strip	_s1zQ_info_itable
	.no_dead_strip	_s1zP_info_itable

