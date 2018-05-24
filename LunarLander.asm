;=====================================================
; LUNAR LANDER by Jim Butterfield
; FIRST BOOK OF KIM
; Pages 84-87
;
	bss
	org	$60
alt	ds	3
vel	ds	3
th2	ds	2
thrust	ds	1
fuel	ds	3
mode	ds	1
down	ds	1
deck	ds	1
;
	code
;	org	$0200
;
; COpy the default data into RAM variables
;
Lunar	ldx	#initend-init	;bytes to copy
lp1	lda	init,x
	sta	alt,x
	dex
	bpl	lp1
;
; Update height and velocity
;
calc	ldx	#5
recal	ldy	#1
	sed
	clc
digit	lda	alt,x
	adc	alt+2,x	;add each digit
	sta	alt,x
	dex
	dey
	bpl	digit
	lda	alt+3,x	;high order
	bpl	incr
	lda	#$99
incr	adc	alt,x
	sta	alt,x
	dex
	bpl	recal
;
	lda	alt
	bpl	up	;still flying?
	lda	#0
	sta	down
	ldx	#2
dd1	sta	alt,x
	sta	th2,x
	dex
	bpl	dd1
up	sec
	lda	fuel+2
	sbc	thrust
	sta	fuel+2
	ldx	#1	;two more digits to go
lp2	lda	fuel,x
	sbc	#0
	sta	fuel,x
	dex
	bpl	lp2
	bcs	tank	;still got fuel?
	lda	#0	;nope, kill motor
	ldx	#3
lp3	sta	thrust,x
	dex
	bpl	lp3
;
; Show altimeter, fuel or messages
;
	jsr	thrset
tank	lda	fuel	;fuel into registers
	ldx	fuel+1
	ora	#$f0	;plus F flag
	ldy	mode
	beq	st
golink	beq	Lunar
clink	beq	calc
	ldx	#$fe
	ldy	#$5a
	clc
	lda	vel+1
	adc	#5
	lda	vel
	adc	#0
	bcs	good
	ldx	#$ad
	ldy	#$de
good	tya
	ldy	down
	beq	st
	lda	alt
	ldx	alt+1
st	sta	POINTH
	stx	POINTL
;
; Show rate of ascent/descent as absolute
;
	lda	vel+1
	ldx	vel	;up or down?
	bpl	fly	;up, we're okay
	sec
	lda	#0
	sbc	vel+1
fly	sta	INH
	lda	#2	;loop twice through display
	sta	deck
;
flite	cld		;display and key test
flite2	jsr	SCANDS	;light 'em up
	jsr	GETKEY	;check keys
	cmp	#$13	;GO key?
	beq	golink	;yes
	bcs	nokey	;if no key
	jsr	dokey
nokey	dec	deck
	bne	flite2	;different from source, but same binary
	beq	clink	;to calc
;
; Subroutine to test keys
;
dokey	cmp	#$0a	;test numeric
	bcc	number
	eor	#$0f	;fuel F gives zero flag
	sta	mode
retrn	rts
;
number	tax
	lda	thrust	;is motor off?
	beq	retrn	;yes, ignore
	stx	thrust	;no, set thrust
;
; Calculate acceleration as thrust minus 5
;
thrset	lda	thrust
	sec
	sed
	sbc	#5
	sta	th2+1
	lda	#0
	sbc	#0
	sta	th2
	rts
;
; Initial values
;
init	db	$45,1,0	;altitude
	db	$99,$81,0	;rate of ascemt
	db	$99,$97	;acceleration
	db	2	;thrust
	db	8,0,0	;fuel
	db	1	;display mode
	db	1	;in flight/landed
initend	equ	*
