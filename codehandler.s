#-----------------------------#
# Machine State Register bits:
# 0 = Disabled 1 = Enabled
#-----------------------------#
# 0xFFFC0000 bit 0-12 = -RESERVED-
# 0x00020000 bit 13 = Power Management
# 0x00010000 bit 15 = Exception Endian Mode
# 0x00008000 bit 16 = Extrenal Interupts
# 0x00004000 bit 17 = Privilege Level
# 0x00002000 bit 18 = Floating Point Enable
# 0x00001000 bit 19 = Machine Check
# 0x00000800 bit 20 = Floating Point Exception Mode 0
# 0x00000400 bit 21 = Single-step trace (Optional)
# 0x00000200 bit 22 = Branch trace (Optional)
# 0x00000100 bit 23 = Floating Point Exception Mode 1
# 0x00000080 bit 24 = -RESERVED-
# 0x00000040 bit 25 = Exception Prefix (0 or F)
# 0x00000020 bit 26 = Instruction Address Translation
# 0x00000010 bit 27 = Data Address Translation
# 0x00000008 bit 28 = -RESERVED-
# 0x00000004 bit 29 = -RESERVED-
# 0x00000002 bit 30 = Recoverable Exception
# 0x00000001 bit 31 = Little-Endian mode

.set ramWrite, TRUE
.set baseAddress, TRUE
.set flowControl, TRUE
.set geckoRegister, TRUE
.set endAndIf, TRUE
.set fSearch, TRUE

.set _handler_end, 0x800018A8 + (regbuffer - file_start)

gameid:
.long 0,0
cheatdata:
.long _handler_end
.space 39*4

.macro push_stack
	stwu r1, -0xE0 (r1)
	stmw r2, 0x24 (r1)
	stw r0, 0x1C (r1)
	mflr r27
	mfcr r28
	mfctr r29
	mfxer r30
	mfmsr r31
	stmw r27, 0x8 (r1)
	ori r31, r31, 0x2000
	andi. r31, r31, 0xF9FF
	mtmsr r31
	stfd f0, 0xC0 (r1)
	stfd f1, 0xC8 (r1)
	stfd f2, 0xD0 (r1)
.endm

.macro pop_stack
	sth r28, 0x4010 (r20)
	lfd f2, 0xD0 (r1)
	lfd f1, 0xC8 (r1)
	lfd f0, 0xC0 (r1)
	lmw r27, 0x8 (r1)
	mtmsr r31
	mtxer r30
	mtctr r29
	mtcr r28
	mtlr r27
	lwz r0, 0x1C (r1)
	lmw r2, 0x24 (r1)
	addi r1, r1, 0xE0
.endm

.macro update_cache register
	dcbst r9, \register
	sync
	icbi r9, \register
	isync
	blr
.endm

.set codeplace, 0x800018A8 + (codelist - file_start)
.set header, 0x80001808
.set broadway, 0xCC00

#START

file_start:

push_stack

lis r31, codeplace@h
lis r20, broadway

lhz r28, 0x4010 (r20)
ori r21, r28, 0xFF
sth r21, 0x4010 (r20) 		#Disable MP3 memory protection

lis r15, 0x8000 			#Intentional extra instruction
ori r15, r15, codeplace@l
ori r7, r31, header@l
mr r6, r31
mr r16, r31
li r8, 0
lis r3, 0xD0
ori r3, r3, 0xC0DE
lwz r4, 0 (r15)
cmpw r3, r4
bne- escape
lwz r4, 0x4 (r15)
cmpw r3, r4
addi r15, r15, 8
beq+ found_codes

escape:
	pop_stack
	isync
	blr

found_codes:
	lwz r3, 0 (r15)
	lwz r4, 0x4 (r15)
	addi r15, r15, 0x8
	andi. r9, r8, 0x1
	cmpwi cr7, r9, 0
	li r9, 0
	rlwinm r10, r3, 3, 29, 31 	#Extract Codetype
	rlwinm r5, r3, 7, 29, 31 	#Extract Sub type
	andis. r11, r3, 0x1000 		#Test Pointer
	rlwinm r3, r3, 0, 7, 31 	#Extract Base Offset
	mr r12, r16
	bne- greater 				#Pointer in use
	rlwinm r12, r6, 0, 0, 6
	greater:
	cmpwi cr4, r5, 0 			#Compares sub code type with 0 in cr4
	
	cmpwi r10, 1
	.if (ramWrite == TRUE)
		blt+ RAM_WRITE 			#Code type 0: Write
	.endif
	
	beq+ IF_STATEMENT 		#Code type 1: Conditional
	
	cmpwi r10, 3
	.if (baseAddress == TRUE)
		blt- BASE_ADDRESS		#Code type 2: Base Address Operation
	.endif
	
	.if (flowControl == TRUE)
		beq FLOW_CONTROL		#Code type 3: Flow control	
	.endif
	
	cmpwi r10, 5
	.if (geckoRegister == TRUE)
		blt- GECKO_REGISTER 	#Code type 4: rGecko Operation
	.endif
	
	.if (endAndIf == TRUE)
		beq+ END_AND_IF 		#Code type 5: Compare [rGecko1] with [rGecko2]
	.endif
	
	cmpwi r10, 7
	blt+ ASM 				#Code type 6: Our baby, the ultimate code type
	
	.if (fSearch == TRUE)	
		b SEARCH_AND_END 			#Code type 7: Misc/End of codelist
	.else
		b escape
	.endif

store_word:
	lwz r18, 0 (r12)
	cmpw r18, r5
	beqlr+
	stw r5, 0 (r12)
	li r9, 1
	b set_cache

store_byte:
	lbzx r18, r9, r12
	rlwinm r0, r4, 0, 24, 31 	#Filter Byte
	cmpw r0, r18
	beqlr+
	stbx r4, r9, r12
	b set_cache

store_halfword:
	lhzx r18, r9, r12
	rlwinm r0, r4, 0, 16, 31 	#Filter Halfword
	cmpw r0, r18
	beqlr+
	sthx r4, r9, r12

set_cache:
	update_cache r12 			#Update the Wii's internal Cache

#CT0=============================================================================
#write  8bits (0): 00XXXXXX YYYY00ZZ
#write 16bits (1): 02XXXXXX YYYYZZZZ
#write 32bits (2): 04XXXXXX ZZZZZZZZ
#string code  (3): 06XXXXXX YYYYYYYY, d1d1d1d1 d2d2d2d2, d3d3d3d3 ....
#Serial Code  (4): 08XXXXXX YYYYYYYY TNNNZZZZ VVVVVVVV

.if (ramWrite == TRUE)
	RAM_WRITE:
		add r12, r12, r3
		cmpwi r5, 3
		beq- move_r9
		bgt- next_line
		bne cr7, found_codes
		cmpwi cr4, r5, 1
		bgt+ cr4, word_store
		rlwinm r10, r4, 16, 16, 31

	store_loop:
		beq cr4, store_halfword
		bl store_byte
		addi r9, r9, 1
		b sub_timer

		bl store_halfword
		addi r9, r9, 2

	sub_timer:
		subic. r10, r10, 1
		bge- store_loop
		b found_codes

	word_store:
		rlwinm r12, r12, 0, 0, 29 	#Make Aligned
		mr r5, r4
		bl store_word
		b found_codes

	move_r9:
		mr r9, r4
		mr r22, r4
		bne- cr7, _skip_and_align
		
	loop_two:
		subic. r9, r9, 1
		blt- _skip_and_align
		lbzx r5, r9, r15
		mr r4, r5
		bl store_byte
		mr r4, r22
		b loop_two

	next_line:
		addi r15, r15, 8
		bne cr7, found_codes
		lwz r5, -0x8 (r15)
		lwz r11, -0x4 (r15)
		rlwinm r17, r5, 0, 16, 31
		rlwinm r10, r5, 16, 20, 31
		rlwinm r5, r5, 4, 28, 31

	loop_three:
		cmpwi cr5, r5, 1
		beql- cr5, store_halfword
		bltl- cr5, store_byte
		ble- iterate
		lwzx r18, r9, r12
		cmpw r4, r18
		beq+ iterate
		stwx r4, r9, r12
		bl set_cache

	iterate:
		add r4, r4, r11
		add r9, r9, r17
		subic. r10, r10, 1
		bge+ loop_three
		b found_codes
.endif

#CT1=============================================================================
#32bits conditional (0,1,2,3): 20XXXXXX YYYYYYYY
#16bits conditional (4,5,6,7): 28XXXXXX ZZZZYYYY

#PS : 31 bit of address = endif.

IF_STATEMENT:
	rlwinm r9, r3, 0, 31, 31 	#r3 = (bit31 & 1) (endif enabled?)
	beq- not_endif 				#Endif not enabled
	rlwinm r8, r8, 31, 1, 31 	#Endif (r8>>1)
	andi. r9, r8, 0x1 			#r9 = Code execution status
	cmpwi cr7, r9, 0 			#Check execution status
	
not_endif:
	cmpwi cr5, r5, 4 			#Subtype 4
	cmpwi cr3, r10, 5 			#Maintype 5
	rlwimi r8, r8, 1, 0, 30 	#r8<<1 and current execution status = old execution status
	bne- cr7, go_loop 			#lf code execution is set to false -> exit
	bgt- cr3, _addresscheck2 	#lf code type==6 -> address check
	add r12, r12, r3 			#address = (ba/po)+(XXXXXX)
	blt- cr3, 12 				#jump lf code type >5 (==1)
	blt- cr5, not_if_equal 		#compare [rN][rM]
	b if_statement
	bge- cr5, load_halfword 	#lf sub code type>=4 -> 16 bits conditional

	rlwinm r12, r12, 0, 0, 29 	#Word Align
	lwz r11, 0 (r12)
	b not_if_equal

load_halfword:
	rlwinm r12, r12, 0, 0, 30 	#Halfword Align
	lhz r11, 0 (r12)

if_statement:
	not r9, r4
	rlwinm r9, r9, 16, 16, 31 	#r9 = extract mask
	and r11, r11, r9 			#r11 &= r9
	rlwinm r4, r4, 0, 16, 31 	#r4 = extract data to check against

not_if_equal:
	cmplw cr6, r11, r4 			#Unsigned compare. r11=data at address, r4=YYYYYYYY
	andi. r9, r5, 0x3
	beq- equal_statement 		#lf sub code (type & 3) == 0
	cmpwi r9, 2
	beq- greater_than_statement #lf sub code (type & 3) == 2
	bgt- less_than_statement 	#lf sub code (type & 3) == 3

	bne- cr6, go_loop 			#If not Equal
	b _skip

equal_statement:
	beq- cr6, go_loop 			#If equal
	b _skip

greater_than_statement:
	bgt- cr6, go_loop 			#If greater than
	b _skip

less_than_statement:
	blt- cr6, go_loop 			#If less than

_skip:
	ori r8, r8, 0x1 			#r8|=1 (execution status set to false)

go_loop:
	bne+ cr3, found_codes 		#lf code type <> 5
	blt+ cr5, found_codes
	lwz r11, -0x8 (r15) 		#load counter
	bne- cr7, store_false_flag 	#lf previous code execution false clear counter
	andi. r12, r3, 0x8 			#else lf clear counter bit not set increase counter
	beq- set_true_flag
	andi. r12, r8, 0x1 			#else lf.. code result true clear counter
	beq- store_false_flag

set_true_flag: 					#else increase the counter
	addi r12, r11, 0x10 		#update counter
	rlwimi r11, r12, 0, 12, 27
	b store_if_flag

store_false_flag:
	rlwinm r11, r11, 0, 28, 11 	#clear the counter

store_if_flag:
	stw r11, -0x8 (r15) 		#save counter
	b found_codes

#CT2============================================================================

#load base address    (0): 40TYZ00N XXXXXXXX = (load/add:T) ba from [(ba/po:Y)+XXXXXXXX(+rN:Z)]

#set base address    (1): 42TYZ00N XXXXXXXX = (set/add:T) ba to (ba/po:Y)+XXXXXXXX(+rN:Z)

#store base address  (2): 440Y0000 XXXXXXXX = store base address to [(ba/po)+XXXXXXXX]
#set base address to (3): 4600XXXX 00000000 = set base address to code address+XXXXXXXX
#load pointer        (4): 48TYZ00N XXXXXXXX = (load/add:T) po from [(ba/po:Y)+XXXXXXXX(+rN:Z)]

#set pointer         (5): 4ATYZ00N XXXXXXXX = (set/add:T) po to (ba/po:Y)+XXXXXXXX(+rN:Y)

#store pointer       (6): 4C0Y0000 XXXXXXXX = store pointer to [(ba/po)+XXXXXXXX]
#set pointer to      (7): 4E00XXXX 00000000 = set pointer to code address+XXXXXXXX

.if (baseAddress == TRUE)
	BASE_ADDRESS:
		bne cr7, found_codes
		rlwinm r9, r3, 2, 26, 29	#r9  = extract N, makes N*4
		rlwinm r14, r3, 16, 31, 31	#r3 = add ba/po flag bit (Y)
		cmpwi cr3, r14, 0
		cmpwi cr4, r5, 4			#cr4 = compare sub code type with 4 (ba/po)
		andi. r14, r5, 0x3			#r14 = sub code type and 3
		cmpwi cr5, r14, 2			#compares sub code type and 2
		blt cr5, _sect1
		beq cr5, _sect2				#sub code type 2

	_psect3:
		extsh r4, r3
		add r4, r4, r15				#r4=XXXXXXXX+r15 (code location in memory)
		b _pend
		
	_sect1:
		rlwinm. r5, r3, 20, 31, 31	#r3 = rN use bit (Z)
		beq +12						#flag is not set(=0), address = XXXXXXXX
		
			lwzx r9, r7, r9			#r9 = load register N
			add r4, r4, r9			#flag is set (=1), address = XXXXXXXX+rN
		
		beq cr3, +8					#(Y) flag is not set(=0), address = XXXXXXXX (+rN)
		
			add r4, r12, r4			#address = XXXXXXXX (+rN) + (ba/po)
		
		cmpwi cr5, r14, 1
		beq cr5, +8					#address = (ba/po)+XXXXXXXX(+rN)
		
			lwz r4, 0 (r4)			#address = [(ba/po)+XXXXXXXX(+rN)]
			
		rlwinm. r3, r3, 12, 31, 31	#r3 = add/replace flag (T)
		beq _pend					#flag is not set (=0), (ba/po)= XXXXXXXX (+rN) + (ba/po)
		
		bge cr4, +12
		
			add r4, r4, r6			#ba += XXXXXXXX (+rN) + (ba/po)
			b _pend
			
		add r4, r4, r16				#po += XXXXXXXX (+rN) + (ba/po)
		b _pend
		
	_sect2:
		rlwinm. r5, r3, 20, 31, 31	#r3 = rN use bit (Z)
		beq +12						#flag is not set(=0), address = XXXXXXXX
		
			lwzx r9, r7, r9			#r9 = load register N
			add r4, r4, r9			#flag is set (=1), address = XXXXXXXX+rN
			
		bge cr4, +12
		
			stwx r6, r12, r4		#[(ba/po)+XXXXXXXX] = base address
			b found_codes
			
		stwx r16, r12, r4			#[(ba/po)+XXXXXXXX] = pointer
		b found_codes
		
	_pend:
		bge cr4, +12
		
			mr r6, r4
			b found_codes			#store result to base address
			
		mr r16, r4					#store result to pointer
		b found_codes
.endif

#CT3============================================================================
#set repeat     (0): 6000ZZZZ 0000000P = set repeat
#execute repeat (1): 62000000 0000000P = execute repeat
#return		(2): 64S00000 0000000P = return (lf true/false/always)
#goto		(3): 66S0XXXX 00000000 = goto (lf true/false/always)
#gosub		(4): 68S0XXXX 0000000P = gosub (lf true/false/always)

.if (flowControl == TRUE)
	FLOW_CONTROL:
		rlwinm r9, r4, 3, 25, 28	#r9  = extract P, makes P*8
		addi r9, r9, 0x40			#offset that points to block P's
		cmpwi r5, 2					#compares sub code type with 2
		blt _repeat
		
		rlwinm. r11, r3, 10, 0, 1
		beq _b_bl_blr
		bgt +8
		b _b_bl_blr_nocheck			#S=2/3, always skip (code exec status turned to true)
		beq- cr7, found_codes		#S=1, skip lf false, don't skip lf true
		b _b_bl_blr_nocheck
		
	_b_bl_blr:
		bne- cr7, found_codes
		
	_b_bl_blr_nocheck:
		cmpwi r5, 3
		bgt- _bl					#sub code type >=4, bl
		beq+ _b						#sub code type ==3, b
		
	_blr:
		lwzx r15, r7, r9			#loads the next code address
		b found_codes
		
	_bl:
		stwx r15, r7, r9			#stores the next code address in block P's address
	_b:
		extsh r4,r3					#XXXX becomes signed
		rlwinm r4, r4, 3, 9, 28
		add r15, r15, r4			#next code address +/-=line XXXX
		b found_codes
		
	_repeat:
		bne- cr7, found_codes		#lf code execution set to false skip code
		add r5, r7, r9				#r5 points to P address
		bne- cr4, _execute_repeat	#branch lf sub code type == 1
		
	_set_repeat:
		rlwinm r4, r3, 0, 16, 31	#r4  = extract NNNN
		stw r15, 0 (r5)				#store current code address to [bP's address]
		stw r4, 0x4 (r5)			#store NNNN to [bP's address+4]
		b found_codes
		
	_execute_repeat:
		lwz r9, 0x4 (r5)			#load NNNN from [M+4]
		cmpwi r9, 0
		beq- found_codes
		subi r9, r9, 1
		stw r9, 0x4 (r5)			#saves (NNNN-1) to [bP's address+4]
		lwz r15, 0 (r5)				#load next code address from [bP's address]
		b found_codes
.endif

#CT4============================================================================
#set/add to rN(0) : 80SY000N XXXXXXXX = rN = (ba/po) + XXXXXXXX
#load rN      (1) : 82UY000N XXXXXXXX = rN = [XXXXXXXX] (offset support) (U:8/16/32)
#store rN     (2) : 84UYZZZN XXXXXXXX = store rN in [XXXXXXXX] (offset support) (8/16/32)

#operation 1  (3) : 86TY000N XXXXXXXX = operation rN?XXXXXXXX ([rN]?XXXXXXXX)
#operation 2  (4) : 88TY000N 0000000M = operation rN?rM ([rN]?rM, rN?[rM], [rN]?[rM])

#copy1        (5) : 8AYYYYNM XXXXXXXX = copy YYYY bytes from [rN] to ([rM]+)XXXXXXXX
#copy2        (6) : 8CYYYYNM XXXXXXXX = copy YYYY bytes from ([rN]+)XXXXXX to [rM]


#for copy1/copy2, lf register == 0xF, base address is used.

.if (geckoRegister == TRUE)
	GECKO_REGISTER:
		bne cr7, found_codes
		rlwinm r11, r3, 2, 26, 29 	#r11  = extract N, makes N*4
		add r26, r7, r11 			#1st value address = rN's address
		lwz r9, 0 (r26) 			#r9 = rN
		rlwinm r14, r3, 12, 30, 31 	#extracts S, U, T (3bits)
		beq cr4, distant 			#lf sub code type = 0
		cmpwi cr4, r5, 5
		bge- cr4, very_far_away 	#lf sub code type = 5/6
		cmpwi cr4, r5, 3
		bge- cr4, farther_away 		#lf sub code type = 5/6
		cmpwi cr4, r5, 1

		#load/store
		rlwinm. r5, r3, 16, 31, 31 	#+(ba/po) flag : Y
		beq no_add 					#address = XXXXXXXX
		add r4, r12, r4
		
	no_add:
		cmpwi cr6, r14, 1
		bne- cr4, idk_do_stuff
		bgt+ cr6, load_word_addr
		beq- cr6, load_half_addr

		lbz r4, 0 (r4) 				#load byte at address
		b store_gecko

	load_half_addr:
		lhz r4, 0 (r4) 				#load halfword at address
		b store_gecko

	load_word_addr:
		lwz r4, 0 (r4) 				#load word at address
		b store_gecko

	idk_do_stuff:
		rlwinm r19, r3, 28, 30, 31 	#r19 = r3 ror 12 (N84UYZZZ)
		mr r12, r4
		mr r4, r9
		mr r5, r9
		li r9, 0

	yet_another_loop:
		bgt+ cr6, is_word
		beq- cr6, is_half
		bl store_byte 				#store byte at address
		subi r12, r12, 1

	is_half:
		beql- cr6, store_halfword 	#store halfword at address
		subi r12, r12, 2

	is_word:
		bgtl+ cr6, store_word 		#store word at address
		addi r12, r12, 4
		subic. r19, r19, 1
		bge+ yet_another_loop
		b found_codes

	distant:
		rlwinm. r5, r3, 16, 31, 31 	#+(ba/po) flag : Y
		beq- dont_add 				#value = XXXXXXXX
		add r4, r4, r12 			#value = XXXXXXXX+(ba/po)

	dont_add:
		andi. r5, r14, 0x1 			#add flag : S
		beq- store_gecko 			#add flag not set (=0), rN=value
		add r4, r4, r9 				#add flag set (=1), rN=rN+value
		b store_gecko

	farther_away:
		rlwinm r10, r3, 16, 30, 31
		rlwinm r14, r4, 2, 26, 29
		add r19, r7, r14
		bne- cr4, not_negative_align
		subi r19, r15, 4

	not_negative_align:
		lwz r4, 0 (r26) 			#Stored value
		lwz r9, 0 (r19)
		andi. r11, r10, 0x1
		beq- no_copy_rFour
		mr r26, r4

	no_copy_rFour:
		andi. r11, r10, 0x2
		beq- cmp_rFive
		mr r19, r9
		bne+ cr4, cmp_rFive
		add r19, r12, r19

	cmp_rFive:
		rlwinm. r5, r3, 12, 28, 31
		cmpwi r5, 9
		bge- check_floats
		bl func_call_one
			add r4, r9, r4			#N + M
		b store_gecko
			mullw r4, r9, r4		#N * M
		b store_gecko
			or r4, r9, r4			#N | M
		b store_gecko
			and r4, r9, r4			#N & M
		b store_gecko
			xor r4, r9, r4			#N ^ M
		b store_gecko
			slw r4, r9, r4			#N << M
		b store_gecko
			srw r4, r9, r4			#N >> M
		b store_gecko
			rlwnm r4, r9, r4, 0, 31		#N rol M
		b store_gecko
			sraw r4, r9, r4			#N asr M
		store_gecko:
			stw r4, 0 (r26)			#Store result in rN/[rN]
		b found_codes

	check_floats:
		cmpwi r5, 10
		bgt+ found_codes
		lfs f1, 0 (r26) 			#f1 = load 1st value
		lfs f2, 0 (r19) 			#f2 = load 2nd value
		fmuls f0, f2, f1			#N = N * M (float)
		beq- floating_multiply
		fadds f0, f2, f1			#N = N + M (float)
	floating_multiply:
		stfs f2, 0 (r26)			#Store result in rN/[rN]
		b found_codes

	func_call_one:
		mflr r10
		rlwinm r5, r5, 3, 25, 28 	#r5 = T*8
		add r10, r10, r5 			#Get pointer to type
		lwz r4, 0 (r26)				#load [rN]
		lwz r9, 0 (r19)				#2nd value address = rM/XXXXXXXX
		mtlr r10
		blr 						#Call operation type
		
	#copy1        (5) : 8AYYYYNM XXXXXXXX = copy YYYY bytes from [rN] to ([rM]+)XXXXXXXX
	#copy2        (6) : 8CYYYYNM XXXXXXXX = copy YYYY bytes from ([rN]+)XXXXXX to [rM]

	very_far_away:
		bne- cr7, found_codes		#lf code execution set to false skip code
		rlwinm r9, r3, 24, 0, 31	#r9=r3 ror 8 (NM8AYYYY, NM8CYYYY)
		mr r14, r12					#r14=(ba/po)
		bl some_sorta_something
		beq- cr4, increment_dest
		add r17, r17, r4			#lf sub code type==0 then source+=XXXXXXXX
		b gecko_stuff

	increment_dest:
		add r9, r9, r4				#lf sub code type==1 then destination+=XXXXXXXX

	gecko_stuff:
		rlwinm. r4, r3, 24, 16, 31	#Extracts YYYY, compares it with 0
		li r5, 0
	wow_another_loop:
		beq found_codes			#Loop until all bytes have been copied.
		lbzx r10, r5, r17
		stbx r10, r5, r9
		addi r5, r5, 1
		cmpw r5, r4
		b wow_another_loop

	some_sorta_something:
		cmpwi cr5, r10, 4			#compare code type and 4(rn Operations) in cr5
		rlwinm r17, r9, 6, 26, 29	#Extracts N*4
		cmpwi r17, 60
		lwzx r17, r7, r17			#Loads rN value in r17
		bne- nope_move
		mr r17, r14					#lf N==0xF then source address=(ba/po)(+XXXXXXXX, CT5)
	nope_move:
		beq- cr5, nope_load
		lhz r17, 0 (r17)			#...and lf CT5 then N = 16 bits at [XXXXXX+base address]
	nope_load:
		rlwinm r9, r9, 10, 26, 29	#Extracts M*4
		cmpwi r9, 60
		lwzx r9, r7, r9				#Loads rM value in r9
		bne- nono_move
		mr r9, r14					#lf M==0xF then dest address=(ba/po)(+XXXXXXXX, CT5)
	nono_move:
		beq cr5, nono_load
		lhz r9, 0 (r9)				#...and lf CT5 then M = 16 bits at [XXXXXX+base address]
	nono_load:
		blr
.endif
	
#CT5============================================================================
#16bits conditional (0,1,2,3): A0XXXXXX NM00YYYY (unknown values)
#16bits conditional (4,5,6,7): A8XXXXXX ZZZZYYYY (counter)

#sub codes types 0,1,2,3 compare [rN] with [rM] (both 16bits values)
#lf register == 0xF, the value at [base address+XXXXXXXX] is used.

.if (endAndIf == TRUE)
	END_AND_IF:
		cmpwi r5, 4
		rlwinm	r11,r3,28,16,31		#extract counter value from r3 in r11
		bge	IF_STATEMENT
		
	_compare16_NM:
		mr	r9,r4					#r9=NM00YYYY
		add	r14,r3,r12				#r14 = XXXXXXXX+(ba/po)
		rlwinm	r14,r14,0,0,30		#16bits align (base address+XXXXXXXX)
		bl some_sorta_something		#r17 = N's value, r9 = M's value
		not	r4, r4					#r4=!r4
		rlwinm	r4,r4,0,16,31		#Extracts !YYYY
		and	r11,r9,r4				#r3 = (M AND !YYYY)
		and	r4,r17,r4				#r4 = (N AND !YYYY)
		b IF_STATEMENT
.endif

#===============================================================================
#execute     (0) : C0000000 NNNNNNNN = execute
#hook1       (1) : C2XXXXXX NNNNNNNN = insert instructions at XXXXXX
#hook2       (3) : C6XXXXXX YYYYYYYY = branch from XXXXXX to YYYYYYYY
#on/off      (6) : CC000000 00000000 = on/off switch
#range check (7) : CE000000 XXXXYYYY = is ba/po in XXXX0000-YYYY0000



ASM:
	mr r26, r4
	rlwinm r4, r4, 3, 0, 28
	bne cr4, _hook_addresscheck	#lf sub code type != 0
	bne- cr7, _skip_and_align
	
_execute:
	sth r28, 0x4010 (r20)
	mtlr r15
	blrl						#C0 Execute
	sth r21, 0x4010 (r20)
	
_skip_and_align:
	add r15, r4, r15
	addi r15, r15, 7
	rlwinm r15, r15, 0, 0, 28 	#Align 64-bit
	b found_codes
	
_hook_addresscheck:
	cmpwi cr4, r5, 3
	bgt cr4, _addresscheck1		#lf sub code type == 6 or 7
	cmpwi r5, 2			#If sub code type == 2
	bne +0x8
	
	ori r3, r3, 1			#set lr bit
	
	lis r5, 0x4800				#Set up branch instruction
	add r12, r3, r12
	rlwinm r12, r12, 0, 0, 29	#align address
	bne- cr4,_hook1			#lf sub code type ==2

_hook2:
	bne- cr7, found_codes
	rlwinm r4, r26, 0, 0, 29 	#address &=0x01FFFFFC
	sub r4, r4, r12				#r4 = to-from
	rlwimi r5, r4, 0, 6, 29		#r5  = (r4 AND 0x03FFFFFC) OR 0x48000000
	rlwimi r5, r3, 0, 31, 31	#restore lr bit
	bl store_word				#stores b at the hook place (over original instruction)
	b found_codes
	
_hook1:
	bne- cr7, _skip_and_align
	sub	r9, r15, r12			#r9 = to-from
	rlwimi r5, r9, 0, 6, 29		#r5  = (r9 AND 0x03FFFFFC) OR 0x48000000
	bl store_word				#stores b at the hook place (over original instruction)
	addi r11, r12, 4
	add	r12, r15, r4
	subi r12, r12, 4			#r12 = address of the last instruction of the hook1 code
	sub	r9, r11, r12
	rlwimi r5, r9, 0, 6, 29		#r5  = (r9 AND 0x03FFFFFC) OR 0x48000000
	bl store_word				#stores b at the last word of the hook1 code
	b _skip_and_align
	
_addresscheck1:
	cmpwi cr4,r5,6
	beq	cr4, _onoff
	b IF_STATEMENT
	
_addresscheck2:
	rlwinm	r12,r12,16,16,31
	rlwinm	r4,r26,16,16,31
	rlwinm	r26,r26,0,16,31
	cmpw r12,r4
	blt	_skip
	cmpw r12,r26
	bge	_skip
	b found_codes

_onoff:
	rlwinm r5,r26,31,31,31		#extracts old exec status (x b a)
	xori r5,r5,1
	andi. r3,r8,1				#extracts current exec status
	cmpw r5,r3
	beq	_onoff_end
	rlwimi r26,r8,1,30,30
	xori r26,r26,2
	rlwinm.	r5,r26,31,31,31		#extracts b
	beq	+8
	xori r26,r26,1
	stw	r26,-4(r15)				#updates the code value in the code list
	
_onoff_end:
	rlwimi	r8,r26,0,31,31		#current execution status = a
	b found_codes

#===============================================================================
#Full terminator  (0) = E0000000 XXXXXXXX = full terminator
#Endlfs/Else      (1) = E2T000VV XXXXXXXX = endlfs (+else)
#End code handler (0) = F0000000 00000000
#Insert ASM XOR16 (1) = F2XXXXXX YYZZZZNN = ASM if XOR = ZZZZ
#Search Pointer   (3) = F60000NN XXXXYYYY = If found, set pointer

.if (fSearch == TRUE)
	SEARCH_AND_END:
		cmpwi r11, 0				#lf code type = 0xF
		beq _notTerminator
		cmpwi r5, 1
		beq _asmTypeba
		cmpwi r5, 2
		beq _asmTypepo
		cmpwi r5, 3
		bne escape
		
	_patchType:
		rlwimi r8, r8, 1, 0, 30		#r8<<1 and current execution status = old execution status
		bne	cr7, _exitpatch			#lf code execution is set to false -> exit
		rlwinm. r23, r3, 22, 0, 1
		bgt _patchfail
		blt _copytopo
		
	_runpatch:
		rlwinm r30, r3, 0, 24, 31
		mulli r30, r30, 2
		rlwinm r23, r4, 0, 0, 15
		xoris r24, r23, 0x8000
		cmpwi r24, 0
		bne- _notincodehandler
		ori r23, r23, 0x3000
		
	_notincodehandler:
		rlwinm r24, r4, 16, 0, 15
		mulli r25, r30, 4
		sub r24, r24, r25
		
	_patchloop:
		li r25, 0

	_patchloopnext:
		mulli r26, r25, 4
		lwzx r27, r15, r26
		lwzx r26, r23, r26
		addi r25, r25, 1
		cmplw r23, r24
		bgt _failpatchloop
		cmpw r25, r30
		bgt _foundaddress
		cmpw r26, r27
		beq _patchloopnext
		addi r23, r23, 4
		b _patchloop
		
	_foundaddress:
		lwz r3, -8 (r15)
		ori r3, r3, 0x300
		stw r3, -8 (r15)
		stw r23, -4 (r15)
		mr r16, r23
		b _exitpatch
		
	_failpatchloop:
		lwz r3, -8 (r15)
		ori r3, r3, 0x100
		stw r3, -8 (r15)
		
	_patchfail:
		ori	r8, r8, 1				#r8|=1 (execution status set to false)
		b _exitpatch
		
	_copytopo:
		mr r16, r4
		
	_exitpatch:
		rlwinm r4, r3, 0, 24, 31 	# set code to number of lines only
		
	_goBackToHandler:
		mulli r4, r4, 8
		add r15, r4, r15 			# skip the lines of the code
		b found_codes
		
	_asmTypeba:
		rlwinm r12, r6, 0, 0, 6 	# use base address
		
	_asmTypepo:
		rlwinm r23, r4, 8, 24, 31 	# extract number of half words to XOR
		rlwinm r24, r4, 24, 16, 31 	# extract XOR checksum
		rlwinm r4, r4, 0, 24, 31 	# set code value to number of ASM lines only
		bne cr7, _goBackToHandler 	# skip code if code execution is set to false
		rlwinm. r25, r23, 0, 24, 24 # check for negative number of half words
		mr r26, r12 				# copy ba/po address
		add r26, r3, r26 			# add code offset to ba/po code address
		rlwinm r26, r26, 0, 0, 29 	# clear last two bits to align address to 32-bit
		beq _positiveOffset 		# if number of half words is negative, extra setup needs to be done
		extsb r23, r23
		neg r23, r23
		mulli r25, r23, 2
		addi r25, r25, 4
		sub r26, r26, r25
		
	_positiveOffset:
		cmpwi r23, 0
		beq _endXORLoop
		li r25, 0
		mtctr r23
		
		_XORLoop:
			lhz r27, 4 (r26)
			xor r25, r27, r25
			addi r26, r26, 2
			bdnz+ _XORLoop
			
		_endXORLoop:
			cmpw r24, r25
			bne _goBackToHandler
			b ASM
		
	_notTerminator:
		bne	cr4, +12				#check lf sub code type == 0
		li r8, 0					#clear whole code execution status lf T=0
		b +20

		rlwinm.	r9, r3, 0, 27, 31	#extract VV
		rlwinm	r5, r3, 12, 31, 31	#extract "else" bit
		srw	r8,r8,r9				#r8>>VV, meaning endlf VV lfs
		rlwinm. r23,r8,31,31,31
		bne _load_baseaddress		# execution is false if code execution >>, so don't invert code status
		xor	r8,r8,r5				#lf 'else' is set then invert current code status

	_load_baseaddress:
		rlwinm.	r5,r4,0,0,15
		beq	+8
		mr	r6,r5					#base address = r4
		rlwinm.	r5,r4,16,0,15
		beq	+8
		mr	r16,r5					#pointer = r4
		b found_codes
.endif

regbuffer:
.space 16*4

.align 3

codelist:
.space 2*4
.end
