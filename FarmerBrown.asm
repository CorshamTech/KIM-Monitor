;=====================================================
; Farmer Brown by Jim Butterfield
; From The First Book of KIM
;
	bss
	org	$60
window	ds	6
wings	ds	6
flag	ds	1
got	ds	1
corn	ds	1
key	ds	1
poinl	ds	1
poinh	ds	1
delay	ds	1
wait	ds	1

	code
;	org	$0200
;
Farmer	ldx	#13	;note: bug in original listing!
	stx	corn	;bushels of corn to start
	lda	#0
sloop	sta	window,x
	dex
	bpl	sloop
test	ldx	#11	;is window empty?
tloop	lda	window,x
	bne	contin	;no, keep going
	dex
	bpl	tloop
	inc	got	;yes, make new animal
	lda	flag
	beq	more	;did last animal get in?
	dec	got
	dec	corn	;take away some corn
	bne	more	;any left?
	jmp	TTYKB
;
more	lda	TIMER	;random value
	lsr	a	;generate new animal
	lsr	a
	lsr	a
	lsr	a
	lsr	a
	cmp	#6	;6 types of animals
	bcc	make
	and	#3
make	clc
	tax		;animal type to X
	adc	#$0a	;key type A to F
	sta	key
	lda	index,x	;animal picture address
	sta	poinl
	lda	#index>>8
	sta	poinh
	ldy	#5	;six locations to move
aloop	lda	(poinl),y	;from picture
	sta	wings,y	;to wings
	dey
	bpl	aloop
	sty	flag	;flag FF = animal coming
contin	ldx	#5
cloop	lda	wings,x	;is animal out of wings?
	bne	nokey2	;no, ignore keyboard
	dex
	bpl	cloop
	jsr	FBROWN1	;undocumented KIM function
	jsr	GETKEY
	cmp	key	;right animal chosen?
	bne	nokey2	;no, ignore key
	lda	flag
	bpl	nokey2	;animal retreating?
	inc	flag	;make it retreat
nokey2	dec	delay	;wait a while...
	bne	nomove	;...before moving animal
	lda	#$20	;speed control value
	sta	delay
	lda	flag	;move animal... which way?
	bmi	coming	;left
	ldx	#10	;right
rloop	lda	window-6,x
	sta	window-5,x
	dex
	bne	rloop
	stx	window-6	;clear extreme left
	beq	nomove	;unconditional branch
;
coming	ldx	#$f0	;-16
cmloop	lda	window+12,x
	sta	window+11,x
	inx
	bmi	cmloop
nomove	lda	#$7f	;light KIM display
	sta	PADD
	ldy	#$13
	ldx	#5
lite	lda	window,x
	sta	SAD
	sty	SBD
litex	inc	wait
	bne	litex
	dey
	dey
	dex
	bpl	lite
	jmp	test
;
; Indices to each animal.  Note that the code
; assumes all of the pictures are in the same
; page!  Yes, the code can be fixed (look for
; references to index) but the original did
; it with just the one-byte offset.
;
index	db	ant&$ff
	db	bird&$ff
	db	cow&$ff
	db	dog&$ff
	db	elefant&$ff
	db	fox&$ff
;
; Animal pictures.  Six bytes per animal.
; Feel free to modify these.
;
ant	db	$08,$00,$00,$00,$00,$00
bird	db	$01,$61,$61,$40,$00,$00
cow	db	$61,$51,$47,$01,$00,$00
dog	db	$63,$58,$4e,$00,$00,$00
elefant	db	$71,$1d,$41,$1f,$01,$00
fox	db	$63,$58,$4c,$40,$00,$00

