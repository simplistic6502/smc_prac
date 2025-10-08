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
	
;Move across incomplete levels
org $238a95
	cmp !Map_PrevMoveDir
	
;Do not persistently track level completions
org $29bca3
	nop
	nop
	nop		;glorious nop's
	
;Do not persistently track fortress completions
org $29bcba
	nop
	nop
	nop		;glorious nop's

; allow world 5 map to scroll
org $238d1c
	nop
	nop
	nop
	nop
	
; limit world 5 map to 2 screens
org $218d08
	db $20 

; do not defeat enemy map objects
org $29b0ab
	bra $5a
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

org $29f180
	;ldy $0300
	;lda $0500
	;pha
	;lsr
	;lsr
	;lsr
	;lsr
	;cmp #$0a
	;bcc $02
	;sbc #$6a
	;clc
	;adc #$f0
	;sta $0304,y
	;pla
	;and #$0f
	;cmp #$0a
	;bcc $02
	;sbc #$6a
	;clc
	;adc #$f0
	;sta $0305,y
	;lda #$00
	;sta $0306,y
	;ldx #$27
	;lda $03ef
	;bne $0f
	;ldx #$2b
	;lda $070a
	;cmp #$10
	;beq $04
	;cmp #$11
	;bne $02
	;ldx #$23
	;txa
	;sta $0301,y
	;lda #$42
	;sta $0302,y
	;lda #$02
	;sta $0303,y
	;lda $0300
	;clc
	;adc #$05
	;sta $0300
	;rts

sub_b579:
	;cmp #$0a
	;bne $02
	;asl
	;asl
	;tay
	;rts
	
sub_b583:
	;ldy $0300
	;lda $f7
	;lsr
	;pha
	;lda #$8d
	;bcc $02
	;lda #$8e
	;sta $0306,y
	;pla
	;lsr
	;pha
	;lda #$8a
	;bcc $02
	;lda #$8b
	;sta $0304,y
	;pla
	;pha
	;and #$03
	;tax
	;lda data_b5e9,x
	;sta $0305,y
	;pla
	;lsr
	;lsr
	;lsr
	;lsr
	;tax
	;lda data_b5e9+4,x
	;sta $0307,y
	;lda #$00
	;sta $0308,y
	;ldx #$27
	;lda $03ef
	;bne $0f
	;ldx #$2b
	;lda $070a
	;cmp #$10
	;beq $04
	;cmp #$11
	;bne $02
	;ldx #$23
	;txa
	;sta $0301,y
	;lda #$22
	;sta $0302,y
	;lda #$04
	;sta $0303,y
	;lda $0300
	;clc
	;adc #$07
	;sta $0300
	;rts
	
data_b5e9:
	;db $85, $86, $87, $89, $80, $82, $83, $84
	
sub_b5f1:
	;tax
	;lda $f7
	;and #$20
	;beq $2f
	;lda $0726
	;beq $02
	;lda #$23
	;adc $03e7
	;adc $03e9
	;tax
	;lda !Inventory_Items,x
	;adc !Debug_Inventory
	;cmp data_b62c,y
	;bne $03
	;lda data_b62c+2,y
	;sta !Inventory_Items,x
	;nop
	;nop
	;nop	;originally JSR $A522
	;lda #$03
	;sta $03ec
	;lda #$0c
	;sta $03e5
	;pla
	;pla
	;rts
	
sub_b627:
	;txa
	;nop
	;nop
	;nop	;originally ADC $A3FC,Y
	;rts
data_b62c:
	;db $0e, $00, $01, $0d

; level timer
org $20f0b7
	jsr Update_LevelTimer
org $20ff78
Update_LevelTimer:
	;LDA $11
	;SEC
	;SBC $1F
	;TAY
	;LDA $65
	;ORA $66
	;BEQ $9FDD
	;LDX $0376
	;BNE $9FD2
	;TYA
	;CLC
	;ADC $0501
	;STA $0501
	;TYA
	;SEC
	;SBC #$01
	;CLC
	;ADC $0500
	;STA $0500
	;LDA $11
	;STA $1F
	inc !Level_TimerAdder
	dec !NMIAckFlag
	rts
	
;World 1 Map Changes
org $2ae29c
	db $46

;World 2 Map Changes
org $2ae348
	db $45
org $2ae351
	db $46
org $2ae366
	db $45, $47, $43
org $2ae371
	db $46
org $2ae38e
	db $45
org $2ae3c9
	db $4a, $45
	
;World 3 Map Changes
org $2ae449
	db $b9
org $2ae45f
	db $b8
org $2ae483
	db $45
org $2ae489
	db $45
org $2ae49f
	db $45, $e8, $45
org $2ae4ba
	db $ba
org $2ae4e0
	db $46
org $2ae4ef
	db $b8

;World 4 Map Changes
org $2ae60e
	db $ac, $ae, $ac, $81, $45
org $2ae616
	db $b3
org $2ae61a
	db $b3
org $2ae67c
	db $45, $bc, $45
org $2ae691
	db $46
org $2ae69b
	db $4a
org $2ae6b5
	db $46
org $2ae6c4
	db $45
org $2ae6f3
	db $9d
	
;World 5 Map Changes
org $2ae720
	db $ba
org $2ae72f
	db $b3
org $2ae733
	db $45, $47
org $2ae755
	db $45, $47, $45, $81, $ac, $dc, $da
org $2ae7c3
	db $da
org $2ae7d8
	db $d7
org $2ae7dc
	db $dc, $da, $dc, $da
	
;World 6 Map Changes
org $2ae83a
	db $45
org $2ae856
	db $b3
org $2ae85a
	db $45
org $2ae8a6
	db $b3
org $2ae8de
	db $b9, $47
org $2ae8e8
	db $45
org $2ae8f1
	db $46
org $2ae906
	db $45
org $2ae96e
	db $b3
org $2ae972
	db $45
	
;World 7 Map Changes
org $2ae9dc
	db $45
org $2ae9eb
	db $46
org $2ae9f3
	db $45, $bc, $45, $81, $50, $b7
org $2aea06
	db $ba
org $2aea16
	db $48
org $2aeaa1
	db $b7, $ae, $ac, $bc, $45
	
;World 8 Map Changes
org $2aeb07
	db $46
org $2aeb12
	db $45, $47, $45, $bc, $45, $4a
org $2aeb12
	db $45, $47, $45, $bc, $45, $4a
org $2aeb96
    db $e2
org $2aeba4
	db $45, $47, $45, $bc, $45, $4a, $45, $47
org $2aebb1
	db $46
org $2aebbb
	db $46
org $2aebc1
	db $48, $45, $4a, $45
org $2aec34
	db $45
org $2aec4b
	db $46
org $2aec6b
	db $46
org $2aece4
	db $45
	
;World 9 Map Changes
org $2aed54
	db $bc
org $2aed64
	db $46, $42, $46, $42, $46, $42, $46
org $2aed74
	db $48
org $2aed88
	db $46, $42, $46
org $2aed98
	db $48

;don't save 1-3 warp whistle collection
org $289428
	nop
	nop
	nop
	nop

;always give first card at end of level
org $28ce15
	sta !Inventory_Cards
	nop

;do not save hit big question blocks (flawed, can recollect while still in level)
org $299955
	nop
	nop
	nop

;player cannot be pulled into hand trap if music box is active or wearing frog suit
org $238c09
	jsr Map_CheckPullHand
org $23a4fc
Map_CheckPullHand:
	lda !Map_MusicBox_Cnt
	bne +
	lda !World_Map_Power,x
	cmp #$04
	beq +
	lda !RandomN,x
	rts
+:  lda #$01
	rts

;do not track immediate level completion
org $29afe6
	nop
	nop

;do not spawn white bonus objects
org $29b192
	nop
	nop
	nop

;disable marching for hammer bros, w7 piranha plants, and coin ships
org $21e697
    db $00,$00,$00,$00,$00,$00
org $21e69f
    db $00

;require player to press A/B to enter map object stages
org $29b8df
	lda !InvFlip_Counter
	bne +
	lda !Pad_Input
	bmi $0b
+:  rts

;do not spawn airship on overworld
org $22f1b5
	lda #$03
	sta !Level_JctCtl
	rts

;treat death like normal level exit
org $209116
	bra $03
	nop
	nop
	nop

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

; remove trailing zero
org $21f0f3 ; normal status bar
	dw $22ad
org $229227 ; closing inventory
	dw $02ad
	
org $20B587
	db $07
	
org $20B918 ; Fix debug menu for SNES
	adc $20a460,x
	and #$07
	sta $03fe
	jmp $b9cd
	lda $18
	and #$20
	beq $0b
	inc $072b
	lda $072b
	and #$01
	sta $072b
	ldx $072b
	lda #$48
	sta $09f0
	lda $20b749,x
	sta $09f1
	lda #$4f
	sta $09f2
	lda #$20
	sta $09f3
	lda #$00
	sta $0a9c
	lda $18
	and #$80
	bra $2c
	
org $20b9e0	; Fix priority of debug menu cursor
	db $20

org $2081ae ; Patch items loading from SRAM (Part 1)
	nop
	nop
	nop

org $2081bb ; Patch items loading from SRAM (Part 2: Electric Boogaloo)
	nop
	nop
	nop
	
org $20b8e7 ; Patch items loading from SRAM (Part 3: WHY IS THERE A LDA #$00 THERE!?)
	nop
	nop
	nop
	
org $29dcf3 ; Do not lose inventory items after use
	bra $3f
	bra $3d