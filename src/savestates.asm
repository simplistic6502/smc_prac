;patch SRAM size checks
org $008060 ;super mario collection
    bra $18
org $03800d ;super mario bros. 1
    bra $18
org $0d800d ;super mario bros.: the lost levels
    bra $18
org $11803c ;super mario bros. 2
    bra $18
org $20a103 ;super mario bros. 3
    bra $18

org $0082ed
    jsr NMI_Hijack

org $0099e5
NMI_Hijack:
    rep #$20            ;use 16-bit accumulator for storing addresses
    lda JoypadAPress,x  ;fetch the address to the first eight bits of buttons pressed
    sta $00             ;this address depends on the game, hence the lookup table
    lda ($00)           ;load the first eight pressed joypad bits
    bit #$0020          ;are we pressing select?
    beq RunNMIRoutine   ;if not, just run the correct NMI routine
    lda JoypadBHeld,x   ;now fetch the address to the second eight bits of buttons held
    sta $00             ;since once again, the address depends on the selected game
    sep #$20            ;return to 8-bit accumulator
    lda ($00)           ;load the second eight held joypad bits
    bit #%00010000      ;are we holding the R shoulder button?
    beq CheckLoadState  ;if not, we aren't saving our state, so branch ahead
    jmp SaveState       ;otherwise run subroutine to save SNES WRAM, VRAM, CGRAM, etc.
CheckLoadState:
    bit #%00100000      ;are we holding the L shoulder button?
    beq RunNMIRoutine   ;if not, just go ahead and run the right NMI routine
    lda $702202         ;are we in the same game that the savestate was made? (very important)
    cmp !SelectedGame
    bne RunNMIRoutine   ;if not, we cannot load this savestate, run NMI routines
    jmp LoadState       ;jump to subroutine to load our savestate
RunNMIRoutine:
    sep #$30            ;restore 8-bit registers no matter what
+:  jmp ($82cb,x)       ;run the appropiate game's NMI routine

JoypadAPress:
    dw !Joypad1APressed_w,!JoypadBitsAPressed,!JoypadBitsAPressed,!Player1JoypadAPress_w,!Controller1APress_w
JoypadBHeld:
    dw !Joypad1BHeld_w,!JoypadBitsBHeld,!JoypadBitsBHeld,!Player1JoypadBHeld_w,!Controller1BHolding_w
ScreenBrightness:
    dw !ScreenBrightness_SMC,!ScreenBrightness_SMB1,!ScreenBrightness_SMB1,!ScreenBrightness_SMB2_w,!ScreenBrightness_SMB3_w
CurrentMusic:
	dw $0066, $1606, $1606, $1DE6, $1206

org $00e5c4
SaveState:
    lda #$01        ;disable further interrupts for the time being
    sta !NMITIMEN   ;however, leave automatic joypad reading on!
	lda #$80
	sta !INIDISP    ;force blanking so we can safely access VRAM
    stz !WMADDH	    ;set WMDATA access to bank $7E for DMA transfer
	rep #$20        ;use 16-bit accumulator to simplify DMA logic
	stz !WMADDL     ;set WMDATA acesss to address $7E0000
	lda #$8080      ;set direction and address increment for transfer
	sta !DMAP0
	ldx #$71        ;set starting SRAM bank for DMA transfer
    lda #$8000      ;set DMA transfer length
SaveWRAM:
    sta !DAS0L      ;transfer $8000 bytes each time
	stz !A1T0L      ;set DMA destination to $xx0000
	stx !A1B0       ;set DMA destination bank
	ldy #$01        ;enable DMA on channel 0
	sty !MDMAEN
    inx             ;move onto next SRAM bank
    cpx #$75        ;unless we've already saved all WRAM, of course
    bcc SaveWRAM    ;in which case, we're done
	ldy #$80        ;increment on reading high byte since A is 16-bit
	sty !VMAIN
    lda #$3981	    ;set direction, increment, and VMDATAREAD source
	sta !DMAP0
    lda #$0000      ;starting VRAM address
SaveVRAM:
	sta !VMADDL     ;store VRAM address here
	lda !VMDATALREAD ;do a dummy read to prep latch
	stz !A1T0L      ;set DMA destination to $xx0000
	stx !A1B0       ;set DMA destination bank
	lda #$8000      ;set DMA transfer length of $8000 bytes
	sta !DAS0L
	ldy #$01        ;enable DMA on channel 0
	sty !MDMAEN
    lda #$4000      ;set VRAM address for second loop
    inx
    cpx #$77        ;if we haven't stored all VRAM, branch
    bcc SaveVRAM    ;otherwise we're done with VRAM
	ldx #$00	    ;start at CGRAM address $00
	stx !CGADD
	lda #$2000
	sta !A1T0L      ;set DMA destination to $xx2000
	ldx #$70        ;set DMA destination bank to $70
	stx !A1B0
	lda #$0200      ;set DMA transfer length of $200 bytes
	sta !DAS0L
	lda #$3b80	    ;set direction, increment, and CGDATAREAD source
	sta !DMAP0
	sty !MDMAEN     ;enable DMA on channel 0 (Y is still $01)
	tsc             ;store the stack pointer in $702200
	sta $702200
    lda !SelectedGame
    tax             ;transfer selected game back to X and store for $702202,
    sta $702202     ;used for a check when trying to load the savestate
	lda #!SAVELOAD_DELAY
-:	ldy !RDNMI      ;clear vblank flag so we don't try to start another NMI
	bpl -
	dec				;delay by a certain amount of frames to give user time to react
	bpl -
	ldy !RDNMI      ;clear vblank flag so we don't try to start another NMI
	lda ScreenBrightness,x
    sta $00         ;store pointer to screen brightness mirror
    sep #$20
    lda ($00)       ;set screen brightness back to what it was before
	sta !INIDISP    ;(probably not necessary in the slightest)
	ldy #$81        ;re-enable NMIs (needed for SMB2 and SMB3 title screen)
	sty !NMITIMEN
	jmp RunNMIRoutine

LoadState:
    lda #$01        ;disable further interrupts for the time being
    sta !NMITIMEN   ;however, leave automatic joypad reading on!
	lda #$80
	sta !INIDISP    ;force blanking so we can safely access VRAM
    stz !WMADDH	    ;set WMDATA access to bank $7E for DMA transfer
	rep #$20        ;use 16-bit accumulator to simplify DMA logic
	stz !WMADDL     ;set WMDATA acesss to address $7E0000
	lda #$8000      ;set direction and address increment for transfer
	sta !DMAP0
	ldx #$71        ;set starting SRAM bank for DMA transfer
    lda #$8000      ;set DMA transfer length
LoadWRAM:
    sta !DAS0L      ;transfer $8000 bytes each time
	stz !A1T0L      ;set DMA destination to $xx0000
	stx !A1B0       ;set DMA destination bank
	ldy #$01        ;enable DMA on channel 0
	sty !MDMAEN
    inx             ;move onto next SRAM bank
    cpx #$75        ;unless we've already loaded all WRAM, of course
    bcc LoadWRAM    ;in which case, we're done
		
	ldy #$80        ;increment on reading high byte since A is 16-bit
	sty !VMAIN
    lda #$1801	    ;set direction, increment, and VMDATA destination
	sta !DMAP0
    lda #$0000      ;starting VRAM address
LoadVRAM:
	sta !VMADDL     ;store VRAM address here
	stz !A1T0L      ;set DMA destination to $xx0000
	stx !A1B0       ;set DMA destination bank
	lda #$8000      ;set DMA transfer length of $8000 bytes
	sta !DAS0L
	ldy #$01        ;enable DMA on channel 0
	sty !MDMAEN
    lda #$4000      ;set VRAM address for second loop
    inx
    cpx #$77        ;if we haven't loaded all VRAM, branch
    bcc LoadVRAM    ;otherwise we're done with VRAM
    ldx #$00	    ;start at CGRAM address $00
	stx !CGADD
	lda #$2000
	sta !A1T0L      ;set DMA destination to $xx2000
	ldx #$70        ;set DMA destination bank to $70
	stx !A1B0
	lda #$0200      ;set DMA transfer length of $200 bytes
	sta !DAS0L
	lda #$2202	    ;set direction, increment, and CGDATA destination
	sta !DMAP0
	sty !MDMAEN     ;enable DMA on channel 0 (Y is still $01)
	lda $702200     ;store $702200 back in the stack pointer
    tcs
    lda !SelectedGame
    tax             ;transfer selected game back to X
	lda CurrentMusic,x
    sta $00         ;store pointer to backup music ID
    sep #$20        ;(this logic isn't fullproof, smb3 switches audio banks)
	lda ($00)       ;load the backup music ID
	cmp !APUIO2     ;is the same as the song playing right now?
	beq +           ;yes, we don't need to do anything
	sta !APUIO2     ;no, force the original song to play instead
+:  rep #$20	    ;16-bit A once more
	lda #!SAVELOAD_DELAY
-:	ldy !RDNMI      ;clear vblank flag so we don't try to start another NMI
	bpl -
	dec				;delay by a certain amount of frames to give user time to react
	bpl -
	lda ScreenBrightness,x
    sta $00         ;store pointer to screen brightness mirror
    sep #$20
    lda ($00)       ;set screen brightness back to what it was before
	sta !INIDISP    ;(probably not necessary in the slightest)
	ldy #$81        ;re-enable NMIs (needed for SMB2 and SMB3 title screen)
	sty !NMITIMEN
-:  jmp RunNMIRoutine

org $00ffd8
    db $08  ;256KB SRAM
    ;original SRAM located at $700000-$701FFF
    ;SNES CGRAM located at $702000-$7021FF
    ;stack pointer located at $702200
    ;selected game located at $702202
    ;SNES WRAM located at $710000-$74FFFF
    ;SNES VRAM located at $750000-$76FFFF
    ;this is basically just what Lui's ROM did, so credit to him
