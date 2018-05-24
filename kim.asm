;- - - - - - - - - - - - - - KIM.ASM - - - - - - - - - - -
; COPYRIGHT MOS TECHNOLOGY, INC
; DATE: OCT 18, 1975 REV-D
;************************ 6530-003 I.C. ******************
; 6530-003 I.C. IS AN AUDIO CASSETTE TAPE RECORDER
; EXTENSION OF THE BASIC KIM MONITOR.  IT FEATURES
; TWO ROUTINES:
;   LOADT - LOAD MEMORY FROM AUDIO TAPE
;     ID=00     IGNORE ID
;     ID=FF     IGNORE ID, USE SA FOR START ADDR
;     ID=01-FE  USE ADDRESS ON TAPE
;
;   DUMPT - STORE MEMORY ONTO AUDIO TAPE
;     ID=00     SHOULD NOT BE USED
;     ID=FF     SHOULD NOT BE USED
;     ID=01-FE  NORMAL ID RANGE
;     SAL       LSB STARTING ADDRESS OF PROGRAM
;     SAH       MSB
;     EAL       ENDING ADDRESS OF PROGRAM
;     EAH       MSB


LOWRAM	equ	0
;
; ASCII characters
;
LF	equ	$0A
CR	equ	$0D
;
; Starting address of Extended KIM monitor
;
EXTKIM	equ	$e000
;
;========================================================
; The VERIFY macro takes one argument which is the
; address this macro should be at.  If not, force an
; error.  Otherwise do nothing.
;
VERIFY	macro	goal
	if	$ < goal
	local	delta
delta	equ	goal-*
	ds	delta
	if	$ > goal
	Error - past target address
	endif
	endif
	endm
;
;========================================================
; The FORCE macro will add bytes to the address so that
; it equals the specified value.
;
FORCE	macro
	endm

TIMER	equ	$1704

SAD	equ	$1740	;6530 A DATA
PADD	equ	$1741	;6530 A DATA DIRECTION
SBD	equ	$1742	;6530 B DATA
PBDD	equ	$1743	;6530 B DATA DIRECTION
CLK1T	equ	$1744	;DIV BY 1 TIME
CLK8T	equ	$1745	;DIV BY 8 TIME
CLK64T	equ	$1746	;DIV BY 64 TIME
CLKKT	equ	$1747	;DIV BY 1024 TIME
CLKRDI	equ	$1747	;READ TIME OUT BIT
CLKRDT	equ	$1746	;READ TIME

	bss
	org	$00ef
;       ** MPU REG.  SAVX AREA IN PAGE 0 **
PCL	ds	1	;PROGRAM CNT LOW
PCH	ds	1	;PROGRAM CNT HI
PREG	ds	1	;CURRENT STATUS REG
SPUSER	ds	1	;CURRENT STACK POINTER
ACC	ds	1	;ACCUMULATOR
YREG	ds	1	;Y INDEX
XREG	ds	1	;X INDEX
;       ** KIM FIXED AREA IN PAGE 0  **
CHKHI	ds	1	;$F6
CHKSUM	ds	1	;$F7
INL	ds	1	;INPUT BUFFER
INH	ds	1	;INPUT BUFFER
POINTL	ds	1	;LSB OF OPEN CELL
POINTH	ds	1	;MSB OF OPEN CELL
TEMP	ds	1
TMPX	ds	1
CHAR	ds	1
MODE	ds	1
;
;       ** KIM FIXED AREA IN PAGE 23 **
;
	if	LOWRAM
	org	$0400
CHKL	ds	1
CHKH	ds	1	;CHKSUM
SAVX	ds	3	;(3-BYTES)
VEB	ds	6	;VOLATILE EXEC BLOCK (6-B)
CNTL30	ds	1	;TTY DELAY
CNTH30	ds	1	;TTY DELAY
TIMH	ds	1
SAL	ds	1	;LOW STARTING ADDRESS
SAH	ds	1	;HI STARTING ADDRESS
EAL	ds	1	;LOW ENDING ADDRESS
EAH	ds	1	;HI ENDING ADDRESS
ID	ds	1	;TAPE PROGRAM ID NUMBER
;       ** INTERRUPT VECTORS **
NMIV	ds	2	;STOP VECTOR (STOP=1C00)
RSTV	ds	2	;RST VECTOR
IRQV	ds	2	;IRQ VECTOR (BRK=1C00)
	else
CHKL	equ	$17E7
CHKH	equ	$17E8	;CHKSUM
SAVX	equ	$17E9	;(3-BYTES)
VEB	equ	$17EC	;VOLATILE EXEC BLOCK (6-B)
CNTL30	equ	$17F2	;TTY DELAY
CNTH30	equ	$17F3	;TTY DELAY
TIMH	equ	$17F4
SAL	equ	$17F5	;LOW STARTING ADDRESS
SAH	equ	$17F6	;HI STARTING ADDRESS
EAL	equ	$17F7	;LOW ENDING ADDRESS
EAH	equ	$17F8	;HI ENDING ADDRESS
ID	equ	$17F9	;TAPE PROGRAM ID NUMBER
;       ** INTERRUPT VECTORS **
NMIV	equ	$17FA	;STOP VECTOR (STOP=1C00)
RSTV	equ	$17FC	;RST VECTOR
IRQV	equ	$17FE	;IRQ VECTOR (BRK=1C00)
	endif

	code
	org	$1800
;
; There needs to be something at 1800 or else the
; resulting binary file is only 1K long and will
; end up at the wrong offset in the EPROM.  If there
; is nothing else here, turn on the text message.
;
;	db	"This space for rent"
	jmp	Lunar
	jmp	Farmer
	include	"LunarLander.asm"
	include "FarmerBrown.asm"
;
; This is an extension of the L command to look for
; Intel hex format files.  If not, keep tossing out
; characters like the original KIM code.
;
LOADIH	cmp	#':'	;Intel hex record start
	beq	LOADIH2	;yes
	jmp	LOAD	;else discard character
;
; It appears to be a hex format record.
;
LOADIH2	lda	#0
	sta	CHKSUM	;clear checksum
	jsr	GETBYT	;get byte count
	tax
	jsr	CHK	;update checksum
	jsr	GETBYT	;get MSB of record
	sta	POINTH
	jsr	CHK
	jsr	GETBYT	;MSB
	sta	POINTL
	jsr	CHK
	jsr	GETBYT	;get record type
;
; This only handles record types 00 (data) and
; 01 (EOF).
;
	cmp	#$01	;EOF?
	beq	LWEOF1	;yes. wait end
;
	cmp	#0
	bne	LWEOF	;unknown type
;
; It's a data record, so get the rest of the
; bytes and save them.
;
	txa		;get back byte count
	beq	LOADIH3	;nothing to get
	ldy	#0
LOADIHN	jsr	GETBYT
	sta	(POINTL),y
	jsr	CHK
	jsr	INCPT	;inc pointer
	dex
	bne	LOADIHN
;
; End of line
;
LOADIH3	jsr	GETBYT	;get checksum
	clc
	adc	CHKSUM
	bne	LOADIHF	;branch if check fail
	jmp	LOAD	;else get next char
;
LWEOF1	jsr	GETBYT	;get checksum
	jmp	START
;
; Wait for end of line
;
LWEOF	jsr	GETCH
	cmp	#CR
	beq	JLOAD
	cmp	#LF
	bne	LWEOF
JLOAD	jmp	LOAD
;
; Load failed
;
LOADIHF	jmp	LOADER

;
;***************************************************
; Text strings moved from the end of the original
; KIM monitor to here where there is more space.
;
TOP	db	LF,CR			;1FD5
OFFCRLF	equ	$-TOP-1
	db	"B0.1v "
	db	"enolC MIK",LF,CR	;KIM
OFFKIM	equ	$-TOP-1
	db	"RRE"			;ERR
OFFERR	equ	$-TOP-1
;
;***************************************************
; This is called from the RST code instead of the
; usual INITS (which this eventually jumps to).
; The extra code here will set up the NMI and IRQ
; vectors to point at the SAVE routine.  This sets
; things up so single-step and BRK work as expected.
;
INITS2	lda	#SAVE&$ff
	sta	NMIV
	sta	IRQV
	lda	#SAVE>>8
	sta	NMIV+1
	sta	IRQV+1
	jmp	INITS
;
;******************* 6530-002 I.C. *****************
;       ** COPYRIGHT MOS TECHNOLOGY INC.
;	  DATE OCT 13, 1975 REV E
;
;       ** KIM **
;	 TTY INTERFACE   6530-002
;	 KEYBOARD INTERFACE,
;	 7-SEGMENT 6-DIGIT DISPLAY
;
;       TTY COMANDS:
;	 G    GOEXEC
;	 CR   OPEN NEXT CELL
;	 LF   OPEN PREVIOUS CELL
;	 .    MODIFY OPEN CELL
;	 SP   OPEN NEW CELL
;	 L    LOAD (OBJECT FORMAT)
;	 Q    DUMP FROM OPEN CELL ADDR TO HI LIMIT
;	 RO   RUB OUT - RETURN TO START KIM
;	       (ALL ILLEGAL CHARS ARE IGNORED)
;
;       KEYBOARD COMMANDS:
;	 ADDR  SETS MODE TO MODIFY CELL ADDRESS
;	 DATA  SETS MODE TO MODIFY DATA IN OPEN CELL
;	 STEP  INCREMENTS TO NEXT CELL
;	 RST   SYSTEM RESET
;	 RUN   GOEXEC
;	 STOP  $1C00 CAN BE LOADED INTO NMIV TO USE
;	 PC    DISPLAY PC (PROGRAM COUNTER)
;

	org	$1C00
SAVE    sta   ACC       ;KIM ENTRY VIA STOP (NMI)      1C00
	pla	     ;OR BRK (IRQ)
	sta   PREG
	pla	     ;KIM ENTRY VIA JSR (A LOST)    1C05
	sta   PCL
	sta   POINTL
	pla	
	sta   PCH
	sta   POINTH
	sty   YREG
	stx   XREG
	tsx
	stx   SPUSER
	jsr   INITS
	jmp   START
;
NMIT    jmp   (NMIV)    ;NON-MASKABLE INTERRUPT TRAP   1C1C
IRQT    jmp   (IRQV)    ;INTERRUPT TRAP		1C1F
RST     ldx   #$FF      ;KIM ENTRY VIA RST	     1C22
	txs
	stx   SPUSER
	jsr   INITS2
	lda   #$FF      ;COUNT sta BIT
	sta   CNTH30    ;ZERO CNTH30
	lda   #$01      ;MASK HI ORDER BITS
DET1    bit   SAD       ;TEST			  1C31
	bne   START     ;KEYBD SSW TEST
	bmi   DET1      ;START BIT TEST
	lda   #$FC
DET3    clc	     ;THIS LOOP COUNTS	      1C3A
	adc   #$01      ;THE START BIT TIME
	bcc   DET2
	inc   CNTH30
DET2    ldy   SAD       ;CHECK FOR END OF START BIT    1C42
	bpl   DET3
	sta   CNTL30
	ldx   #$08
	jsr   GET5      ;GET REST OF THE CHAR, TEST CHAR
;
;       ** MAKE TTY/KB SELECTION **
;
	VERIFY	$1c4f
START	jsr	INIT1
	lda	#$01
	bit	SAD
	bne	TTYKB
	jsr	CRLF	;PRT CR LF
	ldx	#OFFKIM	;TYPE OUT KIM
	jsr	PRTST
	jmp	SHOW1
;
CLEAR	lda	#$00
	sta	INL	;CLEAR INPUT BUFFER
	sta	INH
READ	jsr	GETCH	;GET CHAR
	cmp	#$01
	beq	TTYKB
	jsr	PACK
	jmp	SCAN
;
;       ** MAIN ROUTINE FOR KEYBOARD AND DISPLAY **
;
	VERIFY	$1c77
TTYKB	jsr	SCAND	;IF A=0 NO KEY
	bne	START
TTYKB1	lda	#$01
	bit	SAD
	beq	START
	jsr	SCAND
	beq	TTYKB1
	jsr	SCAND
	beq	TTYKB1
	jsr	GETKEY
	cmp	#$15
	bpl	START
	cmp	#$14
	beq	PCCMD	;DISPLAY PC
	cmp	#$10	;ADDR MODE=1
	beq	ADDRM
	cmp	#$11	;DATA MODE=1
	beq	DATAM
	cmp	#$12	;STEP
	beq	STEP
	cmp	#$13	;RUN
	beq	GOV
	asl	a	;SHIFT CHAR INTO HIGH
	asl	a	;ORDER NIBBLE
	asl	a
	asl	a
	sta	TEMP	;STORE IN TEMP
	ldx	#$04
DATA1	ldy	MODE	;TEST MODE 1=ADDR
	bne	ADDR	;MODE=0 DATA
	lda	(POINTL),y	;GET DATA
	asl	TEMP	;SHIFT CHAR
	rol	a	;SHIFT DATA
	sta	(POINTL),y	;STORE OUT DATA
	jmp	DATA2
;
ADDR    asl   a		;SHIFT CHAR
	rol   POINTL    ;SHIFT ADDR
	rol   POINTH    ;SHIFT ADDR HI
DATA2   dex
	bne   DATA1     ;DO 4 TIMES
	beq   DATAM2    ;EXIT HERE
ADDRM   lda   #$01
	bne   DATAM1
DATAM   lda   #$00
DATAM1  sta   MODE
DATAM2  jmp   START
;
	VERIFY	$1cd3
STEP	jsr	INCPT
	jmp	START
;
GOV	jmp	GOEXEC				  ;1CD9
;
;       ** DISPLAY PC BY MOVING PC TO POINT **
;
PCCMD	lda	PCL				     ;1CDC
	sta	POINTL
	lda	PCH
	sta	POINTH
	jmp	START
;       ** LOAD PAPER TAPE FROM TTY **
LOAD	jsr	GETCH	;LOOK FOR FIRST CHAR	   1CE7
	cmp	#';'	;SEMICOLON
	beq	KLOAD
	jmp	LOADIH	;see if it is Intel hex
;
; Load a KIM format punched tape
;
KLOAD	lda	#$00
	sta	CHKSUM
	sta	CHKHI
	jsr	GETBYT	;GET BYTE COUNT
	tax		;SAVE IN X INDEX
	jsr	CHK	;COMPUTE CHECKSUM
	jsr	GETBYT	;GET ADDRESS HI
	sta	POINTH
	jsr	CHK
	jsr	GETBYT	;GET ADDRESS LO
	sta	POINTL
	jsr	CHK
	txa		;IF CNT=0 DONT
	beq	LOAD3	;GET ANY DATA
LOAD2	jsr	GETBYT	;GET DATA
	sta	(POINTL),y	;STORE DATA
	jsr	CHK
	jsr	INCPT	;NEXT ADDRESS
	dex
	bne	LOAD2
	inx		;X=1 DATA RCD X=0 LAST RCD
LOAD3	jsr	GETBYT	;COMPARE CHKSUM
	cmp	CHKHI
	bne	LOADE1
	jsr	GETBYT
	cmp	CHKSUM
	bne	LOADER
	txa	     ;X=0 LAST RECORD
	bne  	LOAD
	ldx	#OFFKIM	;X-OFF KIM
LOAD8	jsr	PRTST
	jmp	START
;
LOADE1	jsr	GETBYT	;DUMMY
LOADER	ldx	#$11	;X-OFF ERR KIM
	bne	LOAD8
;
;       ** DUMP TO TTY FROM OPEN CELL ADDRESS TO
;	  LIMHL, LIMHH **
;
	VERIFY	$1d42
DUMP	lda	#$00				  ;  1D42
	sta	INL
	sta	INH	;CLEAR RECORD COUNT
DUMP0	lda	#$00
	sta	CHKHI	;CLEAR CHKSUM
	sta	CHKSUM
	jsr	CRLF	;PRINT CR LF
	lda	#$3B	;PRINT SEMICOLON
	jsr	OUTCH
	lda	POINTL	;TEST POINT GT OR ET
	cmp	EAL	;HI LIMIT GOTO EXIT
	lda	POINTH
	sbc	EAH
	bcc	DUMP4
	lda	#$00	;PRINT LAST RECORD
	jsr	PRTBYT	;0 BYTES
	jsr	OPEN
	jsr	PRTPNT
	lda	CHKHI	;PRINT CHKSUM
	jsr	PRTBYT	;FOR LAST RECORD
	lda	CHKSUM
	jsr	PRTBYT
	jmp	CLEAR
;
DUMP4	lda	#$18	;PRINT 24 BYTE COUNT	   1D7A
	tax		;SAVE AS INDEX
	jsr	PRTBYT
	jsr	CHK
	jsr	PRTPNT
DUMP2	ldy	#$00	;PRINT 24 BYTES
	lda	(POINTL),y	;GET DATA
	jsr	PRTBYT	;PRINT DATA
	jsr	CHK	;COMPUTE CHKSUM
	jsr	INCPT	;INCREMENT POINT
	dex
	bne	DUMP2
	lda	CHKHI	;PRINT CHKSUM
	jsr	PRTBYT
	lda	CHKSUM
	jsr	PRTBYT
	inc	INL	;INCR RECORD COUNT
	bne	DUMP3
	inc	INH
DUMP3	jmp	DUMP0
;
	VERIFY	$1da9
SPACE   jsr	OPEN	;OPEN NEW CELL
SHOW    jsr	CRLF	;PRINT CR LF
SHOW1   jsr	PRTPNT
	jsr	OUTSP	;PRINT SPACE
	ldy	#$00	;PRINT DATA SPECIFIED
	lda	(POINTL),y	;BY POINT AD=LDA EXT
	jsr	PRTBYT
	jsr	OUTSP	;PRINT SPACE
	jmp	CLEAR
;
	VERIFY	$1dc2
RTRN	jsr	INCPT	;OPEN NEXT CELL
	jmp	SHOW
;
	VERIFY	$1dc8
GOEXEC	ldx	SPUSER
	txs
	lda	POINTH	;PROGRAM RUNS FROM
	pha		;OPEN CELL ADDRESS
	lda	POINTL
	pha
	lda	PREG
	pha
	ldx	XREG	;RESTORE REGS
	ldy	YREG
	lda	ACC
	rti
;
; This is the TTY command processor.  Check for a
; monitor command and jump to appropriate handler.
;
	VERIFY	$1ddb
SCAN    cmp	#$20	;OPEN CELL
	beq	SPACE
	cmp	#'X'	;Extended KIM monitor
	beq	EKIM
	cmp	#CR	;NEXT CELL
	beq	RTRN
	cmp	#LF	;PREV CELL
	beq	FEED
	cmp	#'.'	;MODIFY CELL
	beq	MODIFY
	cmp	#'G'	;GO EXEC
	beq	GOEXEC
	cmp	#'Q'	;DUMP FROM OPEN CELL TO HI LIMIT
	beq	DUMPV
	cmp	#'L'	;LOAD TAPE
	beq	LOADV
	jmp	READ	;IGNORE ILLEGAL CHAR
;
; JMP instructions for out-of-range branches
;
EKIM	jmp	EXTKIM
DUMPV	jmp	DUMP
LOADV	jmp	LOAD
;
	VERIFY	$1e07
FEED    sec
	lda	POINTL	;DEC DOUBLE BYTE
	sbc	#$01	;AT POINTL AND POINTH
	sta	POINTL
	bcs	FEED1
	dec	POINTH
FEED1	jmp	SHOW
;
MODIFY	ldy	#$00	;GET CONTENTS OF INPUT BUFF
	lda	INL	;INL AND STORE IN LOC
	sta	(POINTL),y	;SPECIFIED BY POINT
	jmp	RTRN
;
;       ** SUBROUTINES FOLLOW **
;
;       SUB TO PRINT POINTL,POINTH
;
	VERIFY	$1e1e
PRTPNT	lda	POINTH	;PRINT POINTL, POINTH	  1E1E
	jsr	PRTBYT
	jsr	CHK
	lda	POINTL
	jsr	PRTBYT
	jsr	CHK
	rts
;
;       **PRINT STRING OF ASCII CHARS FROM TOP+X TO TOP
;
	VERIFY	$1e2f
CRLF	ldx	#OFFCRLF
PRTST	lda	TOP,x
	jsr	OUTCH
	dex
	bpl	PRTST	;STOP ON INDEX ZERO
	rts
;
;       ** PRINT 1 HEX BYTE AS 2 ASCII CHARS **
;
	VERIFY	$1e3b
PRTBYT	sta	TEMP
	lsr	a	;SHIFT CHAR RIGHT 4 BITS
	lsr	a
	lsr	a
	lsr	a
	jsr	HEXTA	;CONVERT TO HEX AND PRINT
	lda	TEMP	;GET OTHER HALF
	jsr	HEXTA	;CONVERT TO HEX AND PRINT
	lda	TEMP	;RESTORE BYTE IN A AND RETURN
	rts
HEXTA	and	#$0F	;MASK HI 4 BITS
	cmp	#$0A
	clc
	bmi	HEXTA1
	adc	#$07	;ALPHA HEX
HEXTA1	adc	#$30	;DEC HEX
	jmp	OUTCH	;PRINT CHAR
;
;       ** GET 1 CHAR FROM TTY, CHAR IN A
;
	VERIFY	$1e5a
GETCH   stx	TMPX	;SAVE X REG     1E5A
	ldx	#$08	;SET UP 8-BIT COUNT
	lda	#$01
GET1    bit	SAD
	bne	GET6
	bmi	GET1	;WAIT FOR START BIT
	jsr	DELAY	;DELAY 1 BIT
GET5    jsr	DEHALF	;DELAY 1/2 BIT TIME
GET2    lda	SAD	;GET 8 BITS
	and	#$80	;MASK OFF LOW ORDER BITS
	lsr	CHAR	;SHIFT RIGHT CHAR
	ora	CHAR
	sta	CHAR
	jsr	DELAY	;DELAY 1 BIT TIME
	dex
	bne	GET2	;GET NEXT CHAR
	jsr	DEHALF	;EXIT THIS ROUTINE
	ldx	TMPX
	lda	CHAR
	rol	a	;SHIFT OFF PARITY
	lsr	a
GET6	rts
;
;       ** INITIALIZATION FOR SIGMA **
;
INITS	ldx	#$01	;SET KB MODE TO ADDR	   1E88
	stx	MODE
INIT1	ldx	#$00
	stx	PADD	;FOR SIGMA USE SADD
	ldx	#$3F
	stx	PBDD	;FOR SIGMA USE SBDD
	ldx	#$07	;ENABLE DATA IN
	stx	SBD	;OUTPUT
	cld
	sei
	rts
;
;       ** PRINT ONE SPACE
;
	VERIFY	$1e9e
OUTSP	lda	#$20	;PRINT SPACE
;
;       ** PRINT ONE CHAR IN A **
;
	VERIFY	$1ea0
OUTCH	sta	CHAR
	stx	TMPX
	jsr	DELAY	;DELAY 10/11 BIT CODE SYNC
	lda	SBD	;START BIT
	and	#$FE
	sta	SBD
	jsr	DELAY
	ldx	#$08
OUT1	lda	SBD	;DATA BIT
	and	#$FE
	lsr	CHAR
	adc	#$00
	sta	SBD
	jsr	DELAY
	dex
	bne	OUT1
	lda	SBD	;STOP BIT
	ora	#$01
	sta	SBD
	jsr	DELAY	;STOP BIT
	ldx	TMPX	;RESTORE INDEX
	rts
;
;       ** DELAY 1 BIT TIME **
;
	VERIFY	$1ed4
DELAY	lda	CNTH30
	sta	TIMH
	lda	CNTL30
DE2	sec
DE4	sbc	#$01
	bcs	DE3
	dec	TIMH
DE3	ldy	TIMH
	bpl	DE2
	rts
;
;       ** DELAY 1/2 BIT TIME **
;
	VERIFY	$1eeb
DEHALF	lda	CNTH30
	sta	TIMH
	lda	CNTL30
	lsr	a
	lsr	TIMH
	bcc	DE2
	ora	#$80
	bcs	DE4
;
;       ** SUB TO DETERMINE IF KEY IS DEPRESSED OR
;	  CONDITION OF SSW KEY NOT DEPRESSED OR
;	  TTY MODE  A=0
;	  KEY DEPRESSED OR KB MODE  A NOT ZERO
;
	VERIFY	$1efe
AK	ldy	#$03	;3 ROWS
	ldx	#$01	;DIGIT 0
ONEKEY	lda	#$FF
AK1	stx	SBD	;OUTPUT DIGIT
	inx		;GET NEXT DIGIT
	inx
	and	SAD	;INPUT SEGMENTS
	dey
	bne	AK1
	ldy	#$07
	sty	SBD
	ora	#$80
	eor	#$FF
	rts
;
;       ** OUTPUT TO 7-SEGMENT DISPLAY **
;
	VERIFY	$1f19
SCAND	ldy	#$00	;GET DATA		      1F19
	lda	(POINTL),y	;SPECIFIED BY POINT
	sta	INH	;SET UP DISPLAY BUFFER
	VERIFY	$1f1f
SCANDS	lda	#$7F	;CHANGE SEG
	sta	PADD	;TO OUTPUT
	ldx	#$09	;INIT DIGIT NUMBER
	ldy	#$03	;OUTPUT 3 BYTES
SCAND1	lda	INL,y	;GET BYTE
	lsr	a	;GET MSD
	lsr	a
	lsr	a
	lsr	a
	jsr	CONVD	;OUTPUT CHAR
	lda	INL,y	;GET BYTE AGAIN
	and	#$0F	;GET LSD
	jsr	CONVD	;OUTPUT CHAR
	dey		;SET UP FOR NEXT BYTE
	bne	SCAND1
	stx	SBD	;ALL DIGITS OFF
;
; Wow, unexpected, but Farmer Brown calls this entry
; point!  It incorrectly lists it as KEYIN but in the
; KIM monitor it has no label.
;
	VERIFY	$1f40
FBROWN1	lda	#$00	;CHANGE SEGMENT
	sta	PADD	;TO INPUTS
	jmp	AK	;GET ANY KEY
;
;       ** CONVERT AND DISPLAY HEX (USED BY SCAND ONLY)**
;
CONVD	sty	TEMP
	tay		;SAVE Y
	lda	TABLE,y	;USE CHAR AS INDEX
	ldy	#$00	;LOOKUP CONVERSION
	sty	SAD	;TURN OFF SEGMENTS
	stx	SBD	;OUTPUT DIGIT ENABLE
	sta	SAD	;OUTPUT SEGMENTS
	ldy	#$7F	;DELAY 500 CYCLES
CONVD1	dey
	bne	CONVD1
	inx		;GET NEXT DIGIT NUMBER
	inx		;ADD 2
	ldy	TEMP	;RESTORE Y
	rts
;
;       ** INCREMENT POINT **
;
	VERIFY	$1f63
INCPT	inc	POINTL
	bne	INCPT2
	inc	POINTH
INCPT2	rts
;
;       ** GET KEY FROM KEYPAD A=KEYVALUE **
;
	VERIFY	$1f6a
GETKEY	ldx	#$21	;START AT DIGIT 0	      1F6A
GETKE5	ldy	#$01	;GET 1 ROW
	jsr	ONEKEY
	bne	KEYIN	;A=0 NO KEY
	cpx	#$27	;TEST FOR DIGIT 2
	bne	GETKE5
	lda	#$15	;15=NOKEY
	rts
KEYIN	ldy	#$FF
KEYIN1	asl	a	;SHIFT LEFT
	bcs	KEYIN2	;UNTIL Y=KEY NO
	iny
	bpl	KEYIN1
KEYIN2	txa
	and	#$0F	;MASK MSD
	lsr	a	;DIVIDE BY 2
	tax
	tya
	bpl	KEYIN4
KEYIN3	clc
	adc	#$07	;MULT (X-1 TIMES A
KEYIN4	dex
	bne	KEYIN3
	rts
;
;       ** COMPUTE CHECKSUM **
;
CHK	clc
	adc	CHKSUM
	sta	CHKSUM
	lda	CHKHI
	adc	#$00
	sta	CHKHI
	rts
;
;       ** GET 2 HEX CHARS AND PACK INTO INL AND INH **
;
	VERIFY	$1f9d
GETBYT	jsr	GETCH
	jsr	PACK
	jsr	GETCH
	jsr	PACK
	lda	INL
	rts
;
;       ** SHIFT CHAR IN A INTO INL AND INH **
;
	VERIFY	$1fac
PACK	cmp	#$30	;CHECK FOR HEX  1FAC
	bmi	UPDAT2
	cmp	#$47	;NOT HEX EXIT
	bpl	UPDAT2
	cmp	#$40	;CONVERT TO HEX
	bmi	UPDATE
	clc
	adc	#$09
UPDATE	rol	a
	rol	a
	rol	a
	rol	a
	ldy	#$04	;SHIFT INTO I/O BUFFER
UPDAT1	rol	a
	rol	INL
	rol	INH
	dey
	bne	UPDAT1
	lda	#$00	;A=0 IF HEX NUM
UPDAT2	rts
;
OPEN	lda	INL	;MOVE I/O BUFFER TO POINT
	sta	POINTL
	lda	INH	;TRANSFER INH- POINTH
	sta	POINTH
	rts
;
;       ** TABLES **
;
;TOP	db	LF,CR			;1FD5
;OFFCRLF	equ	$-TOP-1
;	db	"A0.1v "
;	db	"enolC MIK",LF,CR	;KIM
;OFFKIM	equ	$-TOP-1
;	db	"RRE"			;ERR
;OFFERR	equ	$-TOP-1

	VERIFY	$1fe7
TABLE   db	$BF,$86,$DB,$CF
	db	$E6,$ED,$FD,$87		;0-7
	db	$FF,$EF,$F7,$FC
	db	$B9,$DE,$F9,$F1		;8-F HEX TO 7-SEG
;
;       ** INTERRUPT VECTORS **
;
	org	$1ffa
NMIENT	dw	NMIT
RSTENT	dw	RST
IRQENT	dw	IRQT
	end

