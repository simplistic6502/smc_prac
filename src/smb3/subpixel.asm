;Display X-Subs or Bowser HP instead of Coin Count
org $29e6d0
	ldy #$00
	lda !Level_ObjectID+4
	cmp #$18 ;Bowser Object ID
	beq $09
	lda !Player_XSub_SMB3
	lsr
	lsr
	lsr
	lsr
	bra $1a
	lda !Objects_Hitcount_SMB3+4
	bra $15
	nop
	nop

; replace score on HUD with timer
org $29e79a
	lda !Level_ObjPtr_AddrL
	ora !Level_ObjPtr_AddrH
	bne +
	lda #$a5 ; clock
	sta !StatusBar_Score
	rtl

+:	lda !Level_GetWandState
	beq +
	lda #$a5 ; clock
	sta !StatusBar_Score
	jmp sub_b1d8
+:	lda !Level_TimerFrames
	clc
	adc !Level_TimerAdder
	sta !Level_TimerFrames
	cmp #60
	bcc sub_b1ca
	sbc #60
	sta !Level_TimerFrames
	
	lda !Level_TimerSeconds
	adc #0
	sta !Level_TimerSeconds
	cmp #60
	bcc sub_b1ca
	sbc #60
	sta !Level_TimerSeconds

	lda !Level_TimerMinutes
	adc #0
	cmp #10
	bcc sub_b1c7
	lda #59
	sta !Level_TimerFrames
	lda #59
	sta !Level_TimerSeconds
	lda #$09
sub_b1c7:
	sta !Level_TimerMinutes
sub_b1ca:
	ldx #$a5 ; clock
	lda !Level_TimerFrames
	cmp #30
	bcc +
	ldx #$ad ; blank
+:	stx !StatusBar_Score
sub_b1d8:
	lda #$00
	sta !Level_TimerAdder
	lda !Level_TimerMinutes
	ora #$90
	sta !StatusBar_Score+1
	lda !Level_TimerSeconds
	ldy #$02
	jsr Timer_TwoDigitDisplay
	ldy #$04
	lda !Level_TimerFrames
	jsr Timer_TwoDigitDisplay
	rtl
	;jsr $f180
	;jmp sub_b583
Timer_TwoDigitDisplay:
	ldx #$90
-:	cmp #10
	bcc +
	sbc #10
	inx
	bcs -
+:	ora #$90
	sta !StatusBar_Score+1,y
	txa
	sta !StatusBar_Score,y
	rts

; level timer
org $20f0b7
	jsr Update_LevelTimer
org $20ff78
Update_LevelTimer:
	inc !Level_TimerAdder
	dec !NMIAckFlag
	rts

; remove trailing zero
org $21f0f3 ; normal status bar
	dw $22ad
org $229227 ; closing inventory
	dw $02ad
	
org $20B587
	db $07