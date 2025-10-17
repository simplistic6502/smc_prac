;display player X speed instead of lives
org $29e69a
	lda !Player_XVel
	clc
	adc !Player_SlideRate
	bpl +
	eor #$ff
	adc #$01
+:  ldy #$00
	bra $0a