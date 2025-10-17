; Do not save World Number (breaks overworld enemies)
org $009244
	nop
	nop
	nop
	nop
	
org $009257
	nop
	nop
	nop
	nop
	
; allow world 5 map to scroll
org $238d1c
	nop
	nop
	nop
	nop
	
; limit world 5 map to 2 screens
org $218d08
	db $20 
	
;require player to press A/B to enter map object stages
org $29b8df
	lda !InvFlip_Counter
	bne +
	lda !Pad_Input
	bmi $0b
+:  rts

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

; do not defeat enemy map objects
org $29b0ab
	bra $5a
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