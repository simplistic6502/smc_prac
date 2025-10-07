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