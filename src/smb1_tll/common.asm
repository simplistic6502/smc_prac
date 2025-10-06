;don't increment lives
org $048596
    stz !NumberOfLives

org $04fdef ;use the end of bank $04 for practice functions without common routines
PrintTimerFrame:
    %setup_vram_buffer($7c58,$0100) ;set destination address and length
    sep #$20                        ;8-bit A
    lda !GameTimerCtrlTimer         ;load frames remaining in current timer tick
    pha                             ;push to stack for later
    bne +                           ;if nonzero, branch ahead
    lda #$18                        ;otherwise force to 24
+:  dec                             ;decrement for display
    sta !VRAM_BufferData,x          ;write to buffer
    lda #$20                        ;write high byte to buffer
    sta !VRAM_BufferData+1,x
    %update_buffer_offset($0001)    ;append terminator and update buffer offset
    pla                             ;pull from stack to set Z flag, if relevant
    rtl                             ;return

UpdateLevelTimer:
    lda !LevelTimerFlags        ;check level timer flags for d0 set
    lsr
    bcc UpdateLagCounter        ;if clear, do not modify level timer
    lda !PracticeMenuFlag       ;don't modify level timer if practice menu open
    bne UpdateLagCounter
    ldx #$00                    ;otherwise we need to increment the timer
-:  lda !LevelTimer,x           ;add one to current unit of level timer
    inc
    sta !LevelTimer,x
    cmp #60                     ;is unit over 59?
    bcc UpdateLagCounter        ;no, we're done here
    stz !LevelTimer,x           ;otherwise reset unit to zero
    inx                         ;increment X to add one to next unit of timer
    cpx #$03                    ;abort if trying to increment hour
    bcc -
UpdateLagCounter:
    lda !NMIAckFlag
    beq ++
    lda !OperMode               ;check mode of operation (mostly a copy-paste from practice menu)
    cmp #!VictoryModeValue
    beq +                       ;if victory mode, allow adding to lag counter
    cmp #!GameModeValue         ;if not victory or game mode, cannot add to lag counter
    bne ++           
    lda !OperMode_Task          ;only allow lag counter when game core routine is running
    cmp #$03
    bne ++
+:  lda !PauseModeFlag          ;however, if we're in the normal pause menu, don't add to lag counter
    bne ++
    lda !PracticeMenuFlag       ;...same goes for the practice menu (because that'd be weird)
    bne ++
    inc !LagCounter             ;if we meet these conditions and the game lagged, add to counter
++: lda !NMIAckFlag             ;load NMI acknowledge flag into A to satisfy branch condition on return
    rtl

ChgAreaModeHijack:              ;hijack to store game info on warp zone completion
    lda !WarpZoneControl        ;did we exit level via warpzone?
    beq +                       ;skip custom subroutine if not
    lda !GameEngineSubroutine   ;v1.0.1 fix: previously, if we entered a sideways pipe
    cmp #$03                    ;while WarpZoneControl is set, we erroneously overwrote our saved
    bne +                       ;information...resolved by checking for vertical pipe entry routine
    jsr SaveRNG_EntranceFrame   ;otherwise we took a warp zone and want to save info
    lda !WarpWorldNumber        ;v1.0.1 fix: previously, entering a warp zone pipe would immediately
    sta !WorldNumber            ;overwrite the world and level numbers, breaking quick restart...
    stz !AreaNumber             ;to remedy this, we store the new world number in a seperate address
    stz !LevelNumber            ;and then set the new level when we're about to do the fade-out effect
+:  lda #$00                    ;set operation task and leave
    sta !OperMode_Task
    rtl
LoadAreaPointerHijack_SMB1:
    jsr SaveRNG_EntranceFrame   ;save RNG and other relevant info for stage reload
    jml !LoadAreaPointer_SMB1   ;load area pointer and don't return here
LoadAreaPointerHijack_TLL:
    jsr SaveRNG_EntranceFrame   ;save RNG and other relevant info for stage reload
    jml !LoadAreaPointer_TLL    ;load area pointer and don't return here
SaveRNG_EntranceFrame:
    ldx #$06                    ;init counter for RNG loop 
-:  lda !PseudoRandomBitReg,x   ;copy RNG over
    sta !SavedRNGBytes,x
    dex
    bpl -
    lda !IntervalTimerControl   ;copy entrance frame
    sta !SavedEntranceFrame
    lda !FixFadeoutBGScroll     ;set this depending on type of entrance
    sta !SavedScreenFlag        ;to prevent one-frame RNG desync
    lda !PlayerStatus           ;save power-up state
    sta !SavedPlayerStatus
    rep #$20
    lda !CurrentRNGNumber       ;save RNG number for use in practice menu
    sta !SavedRNGNumber
    sep #$20
    lda !DisableIntermediate    ;save flag used to disable intermediate
    sta !SavedIntermediateFlag  ;if we need to for a x-2 level
    lda !Hidden1UpFlag          ;copy hidden 1-UP flag over for level reload
    sta !Saved1UpFlag
    lda !EntrancePage           ;copy entrance page for level reload
    sta !SavedEntrancePage
    lda !CoinTally              ;copy coin tally for level reload
    sta !SavedCoinTally
    lda #$80                    ;disable operation of level timer but render on lives screen
    sta !LevelTimerFlags
    rts

;RNG enters a 32767-frame loop 39 iterations after seeding...we assign a
;number from $0001 to $7fff for these 32767 combinations.
;if the RNG number is $0000, we haven't entered the loop yet.
StartofRNGLoop:
    db $33,$0f,$69,$77,$a5,$4a,$00  ;RNG number $0001 (aka start of the loop)
UpdateRNGNumber:
    ldx #$06                    ;use X as counter to compare RNG bytes
ChkStartofRNGLoop:
    lda !PseudoRandomBitReg,x   ;compare our current RNG against the start of the RNG loop
    cmp.l StartofRNGLoop,x
    bne NotStartofLoop          ;if any bytes don't match, branch ahead
    dex
    bpl ChkStartofRNGLoop       ;do this for all seven RNG bytes
    rep #$20                    ;if our RNG sequence DOES match, we're at the start of the RNG loop
    lda #$0001                  ;force our RNG number to $0001 to represent this
    sta !CurrentRNGNumber
    bra +                       ;branch ahead to handle the sound engine
NotStartofLoop:
    rep #$20
    lda !CurrentRNGNumber       ;if we aren't at the start of the loop, check if the number
    beq +                       ;is still at $0000 (we haven't done 39 iterations yet)
    inc !CurrentRNGNumber       ;if we are in the loop already, increment the RNG number
+:  sep #$20
    jmp !SoundEngine_w          ;go handle the sound engine now

InitCustomAddresses:
    rep #$20                    ;use 16-bit accumulator
    lda #!DEFAULTADDR_A         ;set default custom addresses on game boot
    sta !CustomAddressA
    lda #!DEFAULTADDR_B
    sta !CustomAddressB
    sep #$20                    ;restore 8-bit accumulator
    jmp !ClearBG3Tilemap_w      ;clear BG3 tilemap and return to previous ROM bank

RenderLevelTimer_SMB1:
    jsl RenderLevelTimer            ;print level timer on the lives screen
    jmp !RenderLevelPreview_SMB1_w  ;then prepare lives screen as we're supposed to

RenderLevelTimer_TLL:
    jsl RenderLevelTimer            ;print level timer on the lives screen
    jml !RenderLevelPreview_TLL     ;then prepare lives screen as we're supposed to

org $05ee5d ;all other practice functions are located at the end of bank $05
PrintRemainderFlagpole:
    phx                             ;preserve X since we'll use it here
    jsr PrintRemainder              ;print the framerule's remainder
    plx                             ;restore X register
    lda #$06                        ;store framerule timer for star flag
    sta !EnemyIntervalTimer,x
    rtl                             ;return
PrintRemainderCastle_8:
    lda #$08                        ;store framerule timer for end of castle (TLL only)
    bra +
PrintRemainderCastle_6:
    lda #$06                        ;store framerule timer for end of castle
+:  sta !WorldEndTimer
    jsr PrintRemainder              ;print remainder
    rtl                             ;return
PrintRemainder:
    %setup_vram_buffer($5958,$0700) ;set destination address and length (print over TIME text)
    lda #$2c1b                      ;"R="
    sta !VRAM_BufferData,x
    lda #$2c5e
    sta !VRAM_BufferData+2,x
    sep #$20                        ;8-bit A so we can set high/low bytes
    lda #$20
    sta !VRAM_BufferData+5,x
    sta !VRAM_BufferData+7,x
    lda !IntervalTimerControl       ;convert remainder to two-digit number and write to buffer
    jsr TwoDigitNumber
    sta !VRAM_BufferData+6,x        ;ones place
    lda $00
    sta !VRAM_BufferData+4,x        ;tens place
    %update_buffer_offset($0007)
UpdateFrameCounterInner:            ;remainder update always redraws frame counter too!
    %setup_vram_buffer($4c58,$0500) ;set destination address and length
    sep #$20                        ;8-bit A so we can set high/low bytes
    lda #$20
    sta !VRAM_BufferData+1,x
    sta !VRAM_BufferData+3,x
    sta !VRAM_BufferData+5,x
    lda !FrameCounter               ;display frame counter as three-digit number
    jsr ThreeDigitNumber
    sta !VRAM_BufferData+4,x        ;ones place
    lda $00
    sta !VRAM_BufferData+2,x        ;tens place
    lda $01
    sta !VRAM_BufferData,x          ;hundreds place
    %update_buffer_offset($0005)
    rts

SidewaysPipeEntry:
    lda #$02                    ;sideways pipe subroutine
    bra +
VerticalPipeEntry:
    lda #$03                    ;vertical pipe subroutine
    bra +
PlayerRdy:
    lda #$01
    sta !LevelTimerFlags        ;re-enable level timer
    lda #$08                    ;player control subroutine
+:  sta !GameEngineSubroutine   ;set player subroutine depending on how we got here
UpdateFrameCounter:
    jsr UpdateFrameCounterInner ;display frame counter on action and return
    rtl
PlayerJumpTimer:
    jsr UpdateFrameCounterInner ;display frame counter
    lda #$20                    ;then set jump/swim timer and return
    sta !JumpSwimTimer
    rtl
PlayerAxeGrab:
    jsr UpdateFrameCounterInner ;display frame counter
    lda #$18                    ;set speed on axe grab and return
    sta !Player_X_Speed
    rtl
BowserSpawn:
    lsr
    sta !BowserMovementSpeed    ;store Bowser movement speed
    phx
    jsr UpdateFrameCounterInner ;display frame counter
    plx
    rtl

BackwardsFlagValue:
    lda !IntervalTimerControl           ;only draw this every four frames
    and #$03                            ;to improve readability and reduce
    cmp #$02                            ;the size of the VRAM buffer
    beq +                               ;jump ahead if d1-d0 of framerule timer =/= $02
    jmp DoScroll
+:  %setup_vram_buffer($6d58,$0300)     ;print backwards flag value where coin count would be
    sep #$30                            ;8-bit registers
    lda #$20                            ;set priority and palette
    sta !VRAM_BufferData+1,x
    sta !VRAM_BufferData+3,x
    lda !LevelNumber                    ;check if we're on a castle stage
    cmp #!Level4
    bne +                               ;if not, print standard backwards flag value
    lda !ScreenRight_X_Pos              ;otherwise, print X coordinate of right edge of the screen
    bra ++                              ;(this is useful for managing camera manipulation in warpless)
+:  lda !Enemy_X_Position+9             ;perform the calculation that causes backwards flag grab
    sec
    sbc !ScreenEdge_X_Pos
++: jsr GetNybbles                      ;get nybbles for result
    sta !VRAM_BufferData+2,x            ;print byte as two-digit hex number
    tya
    sta !VRAM_BufferData,x
    %update_buffer_offset($0003)
    bra +                               ;branch ahead to do sockfolder position
UpdateSockfolderPos:
    lda !IntervalTimerControl           ;only draw this every four frames
    and #$03                            ;to improve readability and reduce
    cmp #$02                            ;the size of the VRAM buffer
    beq +                               ;jump ahead if d1-d0 of framerule timer =/= $02
    jmp DoScroll
+:  lda !SprObject_X_MoveForce                   ; get subpixel x position
    sta $01                                      ; and store it in our temp data
    lda !Player_X_Position                       ; get x position
    sta $00                                      ; and store it in our temp data
    lda !Player_Y_Position                       ; get y position
    eor #$ff                                     ; invert the bits, now $ff is the top of the screen
    lsr                                          ; divide pixel position by 8
    lsr                                          ;
    lsr                                          ;
    bcc +                                        ; if we're on the top half of tile 'tile', we will land 2.5 pixels later.
    pha                                          ; so store the current value
    clc                                          ;
    lda $01                                      ; get subpixel x position
    adc #$80                                     ; and increase it by half
    sta $01                                      ; and store it back
    lda $00                                      ; get x position
    adc #$02                                     ; and add 2 + carry value
    sta $00                                      ; and store it back
    pla                                          ; then restore our original value
+:  sta $02                                      ; store this in our temp value
    asl                                          ; multiply by 4
    asl                                          ;
    adc $02                                      ; and add the temp value
    adc $00                                      ; then add our x position
    sta $00                                      ; thanks threecreepio!
	%setup_vram_buffer($6458,$0500)
    sep #$30                            ;8-bit registers
    lda #$20                            ;set priority and palette for sockfolder display
    sta !VRAM_BufferData+1,x
    sta !VRAM_BufferData+3,x
    sta !VRAM_BufferData+5,x
	lda $00                             ;print the sockfolder bytes sequentially
    jsr GetNybbles
    sta !VRAM_BufferData+2,x
    tya
    sta !VRAM_BufferData,x
	lda $01
    jsr GetNybbles
    tya
    sta !VRAM_BufferData+4,x
    ;we tack on the lag counter display here
    rep #$20
    lda #$7158
    sta !VRAM_BufferAddr+10,x
    lda #$0300
    sta !VRAM_BufferLen+10,x
    sep #$30
    lda #$20
    sta !VRAM_BufferData+11,x
    sta !VRAM_BufferData+13,x
    lda !LagCounter                     ;display lag counter as two-digit hex number
    jsr GetNybbles
    sta !VRAM_BufferData+12,x
    tya
    sta !VRAM_BufferData+10,x
    %update_buffer_offset($000d)        ;update buffer offset appropiately
DoScroll:
    lda !Player_X_Scroll                ;load scroll value for return subroutine
    clc
	rtl

;we take a snapshot of the RNG every 512 frames from the start of the loop
;to limit the number of RNG iterations required for calculation to 511
RNGByte0LookupTable:
    db $33,$b5,$d1,$8c,$1b,$48,$a0,$99,$08,$36,$a7,$42,$fb,$63,$cf,$3b
    db $83,$76,$ce,$e0,$2b,$6f,$a2,$8b,$40,$69,$a2,$d0,$0c,$6d,$b0,$c3
    db $1f,$6c,$30,$27,$02,$12,$48,$5f,$05,$92,$f7,$0e,$7f,$f8,$9c,$1a
    db $fe,$c7,$29,$7d,$ea,$d4,$45,$fb,$55,$de,$73,$95,$2c,$d9,$e1,$ab
RNGByte1LookupTable:
    db $0f,$80,$76,$87,$bf,$ae,$99,$2c,$82,$40,$96,$3e,$4a,$cd,$c4,$8d
    db $c0,$e0,$b9,$f5,$63,$5d,$a1,$42,$a0,$2f,$cb,$29,$90,$65,$cf,$60
    db $cf,$72,$dc,$f3,$38,$6e,$22,$6f,$5d,$17,$da,$a8,$0b,$ed,$0f,$92
    db $65,$06,$5b,$33,$83,$2d,$fd,$38,$11,$81,$9b,$88,$c0,$f2,$aa,$74
RNGByte2LookupTable:
    db $69,$eb,$d4,$9e,$88,$3f,$d8,$1e,$93,$2c,$d9,$ba,$bc,$0a,$5b,$fa
    db $c7,$0d,$24,$34,$35,$83,$e4,$54,$21,$fd,$8e,$89,$89,$bf,$ae,$e6
    db $f0,$aa,$bd,$bc,$3c,$4a,$b2,$d1,$57,$33,$35,$b5,$f5,$1c,$37,$a7
    db $99,$88,$09,$c9,$56,$85,$76,$ce,$bb,$3c,$7c,$a3,$99,$41,$69,$22
RNGByte3LookupTable:
    db $77,$ea,$39,$91,$f7,$63,$eb,$46,$96,$ac,$f4,$c7,$29,$90,$d3,$e1
    db $46,$cd,$56,$de,$f3,$38,$a7,$d0,$61,$a2,$19,$da,$a8,$74,$31,$27
    db $6f,$4f,$04,$5b,$4c,$96,$f7,$0e,$ed,$1d,$81,$e4,$e2,$c6,$29,$82
    db $52,$85,$bf,$ae,$50,$de,$8c,$bf,$98,$3e,$4a,$b2,$18,$a5,$3d,$ca
RNGByte4LookupTable:
    db $a5,$3d,$91,$ac,$e6,$1d,$5a,$7a,$b1,$f5,$47,$b2,$51,$85,$64,$14
    db $c8,$d6,$1e,$b7,$98,$3e,$6e,$79,$23,$59,$05,$c9,$bb,$0a,$6d,$eb
    db $8f,$1b,$7e,$23,$34,$03,$92,$ac,$42,$7b,$ea,$8f,$09,$ff,$47,$cd
    db $60,$94,$ac,$3d,$fc,$d5,$61,$22,$ef,$46,$b2,$f5,$2a,$26,$ef,$8f
RNGByte5LookupTable:
    db $4a,$e9,$e2,$8f,$09,$db,$8c,$f6,$9c,$ac,$af,$3d,$03,$a4,$c2,$d6
    db $45,$4d,$b2,$0a,$7f,$4e,$20,$d9,$e1,$1d,$37,$7c,$ea,$e2,$0f,$a4
    db $50,$85,$76,$95,$ac,$2f,$7d,$b1,$98,$41,$e9,$46,$cd,$72,$15,$c8
    db $c4,$9f,$d3,$61,$5d,$68,$79,$5c,$de,$3a,$27,$90,$1a,$6c,$94,$1a
RNGByte6LookupTable:
    db $00,$92,$c1,$d6,$c5,$e0,$39,$03,$ff,$47,$20,$59,$a1,$af,$0b,$ff
    db $d5,$e1,$8f,$64,$4f,$32,$fc,$2a,$a6,$af,$3d,$ee,$9d,$f7,$d5,$73
    db $4e,$b2,$8a,$d2,$c5,$29,$59,$e8,$1d,$b7,$3c,$58,$de,$8c,$9b,$53
    db $05,$b6,$8a,$1b,$a5,$c2,$bb,$18,$01,$b6,$43,$7b,$4e,$20,$4b,$04
SetRNGFromNumber:
    rep #$30                    ;set 16-bit accumulator and X/Y registers
    lda !MenuRNGNumber          ;is the menu RNG number $0000?
    bne DoNormalRNG             ;if not, start the number-to-RNG conversion
    lda !CurrentRNGNumber       ;otherwise we'll just copy the current RNG
    sta !SavedRNGNumber         ;to our saved copy used for level reload
    sta !MenuRNGNumber          ;also copy it here to show the new RNG number
    sep #$30                    ;restore 8-bit registers
    ldx #$06                    ;start counter for RNG transfer
-:  lda !PseudoRandomBitReg,x   ;copy the RNG over now
    sta !SavedRNGBytes,x
    dex
    bpl -                       ;repeat until we've copied all bytes
    bra DoneWithRNG             ;now skip over the normal RNG calculation
DoNormalRNG:
    sta !SavedRNGNumber         ;copy RNG number over to saved copy for level reload
    dec                         ;decrement our RNG number (since $0000 is our "random" option)
    pha                         ;push to stack for later
    and #$7e00                  ;mask out all bits but d14-d9
    lsr                         ;shift bits over to d5-d0
    lsr
    lsr
    lsr
    lsr
    lsr
    lsr
    lsr
    lsr
    tax                         ;transfer to X to use as offset for RNG lookup tables
    sep #$20                    ;use 8-bit accumulator for now
    lda RNGByte0LookupTable,x   ;use lookup table to set base for RNG
    sta !SavedRNGBytes
    lda RNGByte1LookupTable,x
    sta !SavedRNGBytes+1
    lda RNGByte2LookupTable,x
    sta !SavedRNGBytes+2
    lda RNGByte3LookupTable,x
    sta !SavedRNGBytes+3
    lda RNGByte4LookupTable,x
    sta !SavedRNGBytes+4
    lda RNGByte5LookupTable,x
    sta !SavedRNGBytes+5
    lda RNGByte6LookupTable,x
    sta !SavedRNGBytes+6
    rep #$20                    ;restore 16-bit accumulator now that we're done
    pla                         ;pop RNG number off stack
    and #$01ff                  ;mask out all but d8-d0
    tax                         ;transfer result to X to use as counter for loop
    sep #$20                    ;use 8-bit accumulator once again
    cpx #$0000                  ;is our counter value at zero?
    beq DoneWithRNG             ;if so, don't bother doing any RNG iteration
AdvanceRNGLoop:                 ;otherwise iterate through RNG until the counter hits zero
    lda !SavedRNGBytes          ;XOR current d49 and d41
    and #%00000010
    sta $00
    lda !SavedRNGBytes+1
    and #%00000010
    eor $00
    clc                         ;clear or set carry based on result
    beq +
    sec
+:  ror !SavedRNGBytes          ;shift all bits to the right,
    ror !SavedRNGBytes+1        ;discard the current d0, and set
    ror !SavedRNGBytes+2        ;the new d55 based on the earlier XOR
    ror !SavedRNGBytes+3
    ror !SavedRNGBytes+4
    ror !SavedRNGBytes+5
    ror !SavedRNGBytes+6
    dex
    bne AdvanceRNGLoop
DoneWithRNG:
    sep #$30
    lda !MenuEntranceFrame      ;copy other entrance values from menu to saved copies
    sta !SavedEntranceFrame
    lda !MenuScreenFlag
    sta !SavedScreenFlag
    rts

UpdateCustomAddresses:
    ldx !VRAM_BufferOffset   ;only do this if VRAM buffer is empty (to limit buffer size)
    bne +
    ldx !PauseModeFlag       ;skip over this if pause menu is open because there's no point in updating
    bne +
    ldx !DisableScreenFlag   ;do not update custom addresses if screen disable flag is set
    bne +                    ;(prevents VRAM glitches when overwriting mario's name with luigi's)
    rep #$30                 ;enable 16-bit registers
    lda #$5558               ;location of text, LSB then MSB
    sta !VRAM_BufferAddr
    lda #$0500               ;length of text, LSB then MSB
    sta !VRAM_BufferLen
    lda !CustomAddressA      ;store address as pointer
    sta $02
    sep #$30
    lda #$20                 ;set all upper bytes in one fell swoop
    sta !VRAM_BufferData+1
    sta !VRAM_BufferData+3
    sta !VRAM_BufferData+5
    sta !VRAM_BufferData+11
    sta !VRAM_BufferData+13
    sta !VRAM_BufferData+15
    lda ($02)                ;fetch value of address referenced by RAM address A
    jsr ThreeDigitNumber     ;print as three digit number
    sta !VRAM_BufferData+4
    lda $00
    sta !VRAM_BufferData+2
    lda $01
    sta !VRAM_BufferData
    rep #$30                 ;enable 16-bit registers
    lda #$7558               ;location of text, LSB then MSB
    sta !VRAM_BufferAddr+10
    lda #$0500               ;length of text, LSB then MSB
    sta !VRAM_BufferLen+10
    lda !CustomAddressB      ;store address as pointer
    sta $02
    sep #$30
    lda ($02)                ;fetch value of address referenced by RAM address B
    jsr ThreeDigitNumber     ;print as three digit number
    sta !VRAM_BufferData+14
    lda $00
    sta !VRAM_BufferData+12
    lda $01
    sta !VRAM_BufferData+10
    lda #$ff                 ;apply terminator to end of VRAM buffer
    sta !VRAM_BufferData+16
    txa
    clc
    adc #20
    sta !VRAM_BufferOffset   ;update VRAM buffer offset accordingly
+:  rts

RenderLevelTimer:
    lda !LevelTimerFlags            ;if d7 clear, don't render level timer
    bpl +
    %setup_vram_buffer($cc58,$0f00) ;set destination address and length
    lda #$2061                      ;initialize level timer string
    sta !VRAM_BufferData,x
    sta !VRAM_BufferData+2,x
    sta !VRAM_BufferData+4,x
    sta !VRAM_BufferData+6,x
    sta !VRAM_BufferData+8,x
    lda #$2062
    sta !VRAM_BufferData+10,x
    sta !VRAM_BufferData+12,x
    sta !VRAM_BufferData+14,x
    sep #$20                        ;set 8-bit accumulator
    lda !LevelTimer                 ;print frames
    jsr TwoDigitNumber
    sta !VRAM_BufferData+14,x
    lda $00
    sta !VRAM_BufferData+12,x
    lda !LevelTimer+1               ;print seconds
    jsr TwoDigitNumber
    sta !VRAM_BufferData+8,x
    lda $00
    sta !VRAM_BufferData+6,x
    lda !LevelTimer+2               ;print minutes
    jsr TwoDigitNumber
    sta !VRAM_BufferData+2,x
    lda $00
    sta !VRAM_BufferData,x
    %update_buffer_offset($000f)    ;append terminator and update buffer offset
+:  stz !LevelTimer                 ;reset level timer for the next stage
    stz !LevelTimer+1
    stz !LevelTimer+2
    rtl

PracticeMenu_SMB1:
    jsl !ReadJoypads_SMB1       ;read controllers
    stz !CurrentGame            ;use variable to indicate we're in smb1
    jsr RunPracticeRoutines     ;run custom practice subroutines
    lda !PracticeMenuFlag       ;if we have the menu enabled, only run audio subs
    bne SkipGameLoop_SMB1
    lda !DelayRNGFlag           ;do we have to delay RNG because FixFadeoutBGScroll is zero?
    beq Return_SMB1             ;if not, do normal game execution
    jsl !SoundEngine            ;otherwise run audio subs
    pla                         ;then pop return address off stack
    pla
    pla
    stz !DelayRNGFlag           ;clear flag
    jml !SkipRNGAndSound_SMB1   ;and jump here to skip RNG update
Return_SMB1:
    rtl
SkipGameLoop_SMB1:
    jsl !SoundEngine            ;otherwise run audio subs
    pla                         ;then pop return address off stack
    pla
    pla
    jml !WaitForNMI_SMB1        ;and jump to wait for next game loop
PracticeMenu_TLL:
    jsl !ReadJoypads_TLL        ;read controllers
    lda #$01
    sta !CurrentGame            ;use variable to indicate we're in tll
    jsr RunPracticeRoutines     ;run custom practice subroutines
    lda !PracticeMenuFlag       ;if we have the menu enabled, only run audio subs
    bne SkipGameLoop_TLL
    lda !DelayRNGFlag           ;do we have to delay RNG because FixFadeoutBGScroll is zero?
    beq Return_TLL              ;if not, do normal game execution
    jsl !SoundEngine            ;otherwise run audio subs
    pla                         ;then pop return address off stack
    pla
    pla
    stz !DelayRNGFlag           ;clear flag
    jml !SkipRNGAndSound_TLL    ;and jump here to skip RNG update
Return_TLL:
    rtl
SkipGameLoop_TLL:
    jsl !SoundEngine            ;otherwise run audio subs
    pla                         ;then pop return address off stack
    pla
    pla
    jml !WaitForNMI_TLL         ;and jump to wait for next game loop

RunPracticeRoutines:
    rep #$20                    ;v1.0.1 fix: due to an oversight, $02 getting overwritten
    lda $00                     ;breaks the physics of the 6-4 and B-4 ending sequence, since
    pha                         ;$02 is used to store the max speed of the toads...
    lda $02                     ;to err on the side of caution, push all temp RAM we use
    pha                         ;to the stack, so that way we can restore it when we're done
    sep #$20                    ;note that we currently reserve $00-$03 for use as scratch RAM
    jsr HandlePracticeMenu      ;take care of the practice menu or quick restart, if needed
    jsr UpdateCustomAddresses   ;update the custom LowRAM address values
    rep #$20                    ;set 16-bit accumulator so we can retrieve temp RAM correctly
    pla                         ;pull $02-$03 from the stack and restore
    sta $02
    pla                         ;pull $00-$01 from the stack and restore
    sta $00
    sep #$20                    ;return to 8-bit accumulator since we've restored temp RAM
    rts                         ;we're done with the practice routines at this point
HandlePracticeMenu:
    lda !PracticeMenuFlag       ;are we currently in the practice menu?
    beq CheckForMenu
    jmp CheckForMenuClose       ;if we are, go ahead and run relevant code
CheckForMenu:
    lda !OperMode               ;check mode of operation
    cmp #!VictoryModeValue
    beq +                       ;if victory mode, allow practice menu
    cmp #!GameModeValue         ;if not victory or game mode, practice menu cannot be opened
    bne ExitMenuCheck           
    lda !OperMode_Task          ;only allow practice menu when game core routine is running
    cmp #$03
    bne ExitMenuCheck
+:  lda !PauseModeFlag          ;however, if we're in the normal pause menu, don't open practice menu
    bne ExitMenuCheck
    lda !FixFadeoutBGScroll     ;check if we're doing a fade-out or fade-in
    bne ExitMenuCheck           ;yes, leave
    lda !JoypadBitsBHeld        ;are we holding either L or R (or both)?
    bit #%00110000
    beq +                       ;if so, we're using savestates or doing level restart, so branch elsewhere
    and #%00110000              ;now check for quick restart, which uses the L+R shoulder buttons
    cmp #%00110000
    bne ExitMenuCheck           ;if we aren't holding those buttons, leave
    jmp ForceLevelReload        ;no, restart the current level with RNG and entrance frame intact
+:  lda !JoypadBitsAPressed     ;after all this, check if we're pressing select to open the menu
    and #%00100000
    cmp #%00100000
    beq PreparePracticeMenu     ;if we are, prepare the menu
ExitMenuCheck:
    rts           
PreparePracticeMenu:
    inc !PracticeMenuFlag       ;set flag to indicate menu is open
    inc !DrawOptionFlag         ;set flag to draw the currently-selected option
    lda !PrimaryHardMode        ;copy relevant game info to menu-related memory
    sta !MenuHardModeFlag
    lda !WorldNumber
    sta !MenuWorldNumber
    lda !LevelNumber
    sta !MenuLevelNumber
    lda !PlayerStatus
    sta !MenuPlayerStatus
    lda !SelectedPlayer
    sta !MenuSelectedPlayer
    lda !CoinTally
    sta !MenuCoinTally
    lda !WarpsUsedFlag
    sta !MenuWarpsFlag
    rep #$20
    lda !SavedRNGNumber
    sta !MenuRNGNumber
    sep #$20
    lda !SavedEntranceFrame
    sta !MenuEntranceFrame
    lda !SavedScreenFlag
    sta !MenuScreenFlag
    lda !Saved1UpFlag           ;copy saved 1-UP flag over
    cmp #$02                    ;check if it's greater than $01
    bcc +                       ;if it isn't, use value as-is
    lda #$01                    ;otherwise force to $01
+:  sta !Menu1UpFlag            ;store 1-UP flag here
    stz !MenuSelectionIndex     ;clear menu selection index
    %menu_open_sfx()            ;play sfx for opening menu
    %setup_vram_buffer($2258,$0100)
    lda #$2060
    sta !VRAM_BufferData,x
    %update_buffer_offset($0001)
    %setup_vram_buffer($3d58,$0100)
    lda #$6060
    sta !VRAM_BufferData,x
    %update_buffer_offset($0001)
    bra RunPracticeMenu         ;run the practice menu code to immediately draw option

CheckForMenuClose:
    lda !JoypadBitsBHeld        ;are we holding L or R shoulder buttons?
    bit #%00110000              ;if we are, we're either doing menu navigation or
    bne RunPracticeMenu         ;trying to use savestates, so just run the menu
    lda !JoypadBitsAPressed     ;check if we're pressing select to close the menu
    bit #%00100000
    beq RunPracticeMenu         ;if we aren't, run the menu
    %menu_close_sfx()           ;play sfx if we're closing the menu
ForceMenuClose:
    stz !PracticeMenuFlag       ;clear practice menu flag
    %setup_vram_buffer($2258,$3740)
    lda #$2028                  ;clear out the row above the status bar
    sta !VRAM_BufferData,x
    %update_buffer_offset($0001)
    rts                         ;return to normal operation
RunPracticeMenu:
    phb                         ;push current data bank to stack
    phk                         ;store program bank in data bank
    plb
    jsr PracticeMenuCore        ;do practice menu stuff
    plb                         ;pull data bank from stack and leave
LeavePracticeMenu:
    rts

PracticeMenuCore:
    lda !DrawOptionFlag         ;do we need to draw something?
    beq ControlPracticeMenu     ;no, check for menu navigation
    lda !VRAM_BufferOffset      ;if we need to draw something, wait for VRAM buffer to empty
    bne LeavePracticeMenu
    stz !DrawOptionFlag         ;now clear draw flag
    lda !PracticeMenuOption     ;get the option we've selected
    pha                         ;push to stack for later
    asl
    tax                         ;multiply by two for offset
    rep #$20                    ;16-bit accumulator
    lda OptionTextPointers,x    ;fetch the appropiate buffer to store
    sta $00                     ;store pointer to it
    lda #$2358                  ;location of text, LSB then MSB
    sta !VRAM_BufferAddr
    lda #$3300                  ;length of text, LSB then MSB
    sta !VRAM_BufferLen
    ldy #$34                    ;hardcoded length, yay!
-:  lda ($00),y                 ;copy to VRAM buffer
    sta !VRAM_BufferData,y
    dey
    dey
    bpl -
    sep #$20                    ;8-bit accumulator
    lda #$ff                    ;apply terminator to end of VRAM buffer
    sta !VRAM_BufferData+$34
    clc
    adc #$38
    sta !VRAM_BufferOffset      ;update VRAM buffer offset accordingly
    pla                         ;run code to finalize buffer depending on option
    asl
    tax
    jmp (OptionDrawPointers,x)
OptionTextPointers:
    dw LevelOptionText
    dw PowerupOptionText
    dw EntranceOptionText
    dw CoinsOptionText
    dw AddressOptionText
    dw PlayerOptionText
    dw WarpsOptionText
    dw Hidden1UpOptionText
    dw InvincibilityOptionText
    dw TitleOptionText
OptionDrawPointers:
    dw DrawLevelOption
    dw DrawPowerupOption
    dw DrawEntranceOption
    dw DrawCoinsOption
    dw DrawAddressesOption
    dw DrawPlayerOption
    dw DrawWarpsOption
    dw Draw1UpOption
    dw DrawOptionStub
    dw DrawOptionStub

ControlPracticeMenu:
    lda !JoypadBitsBPressed     ;check if L or R were pressed
    bit #%00010000
    bne RPress                  ;go here if R was pressed
    bit #%00100000
    bne LPress                  ;go here if L was pressed
    bra ControlOption           ;otherwise control selected option
RPress:
    lda !PracticeMenuOption     ;increment to next option
    inc
    cmp #!MAX_OPTIONS           ;are we past the max option allowed?
    bcc +                       ;no, set option number
    lda #$00                    ;yes, wrap option number around to zero
+:  sta !PracticeMenuOption     ;store our menu option here
    tay                         ;move menu option to Y register to use as index
    lda OptionAllowedGame,y     ;use lookup table to check which games
    cmp #!BOTH_GAMES            ;will allow this menu option to be selected
    bcs DrawNewOption           ;if both games allow it, branch ahead
    cmp !CurrentGame            ;otherwise check which game allows it
    bne RPress                  ;if not allowed, advance to the next option
    bra DrawNewOption           ;otherwise branch to draw the option
LPress:
    lda !PracticeMenuOption     ;decrement to previous option
    dec
    bpl +                       ;are we past the first option?
    lda #!MAX_OPTIONS           ;if we are, get the max number of options
    dec                         ;and decrement to go to last menu option
+:  sta !PracticeMenuOption     ;store our menu option here
    tay                         ;move menu option to Y to use as index
    lda OptionAllowedGame,y     ;use lookup table to check which games
    cmp #!BOTH_GAMES            ;allow this menu option to be selected
    beq DrawNewOption           ;if both games allow it, branch ahead
    cmp !CurrentGame            ;otherwise check which game allows it
    bne LPress                  ;if not allowed, go back another option
DrawNewOption:
    %change_option_sfx()        ;play option change sound effect
    inc !DrawOptionFlag         ;set flag to draw the new option
    stz !MenuSelectionIndex     ;clear selection index within option
    rts                         ;leave
ControlOption:
    lda !PracticeMenuOption     ;control whatever option we're currently on
    asl
    tax
    jmp (OptionCtrlPointers,x)
OptionCtrlPointers:
    dw ControlLevelOption
    dw ControlPowerupOption
    dw ControlEntranceOption
    dw ControlCoinsOption
    dw ControlAddressesOption
    dw ControlPlayerOption
    dw ControlWarpsOption
    dw Control1UpOption
    dw ControlInvincibilityOption
    dw ControlTitleOption
OptionAllowedGame:
    db !BOTH_GAMES
    db !BOTH_GAMES
    db !BOTH_GAMES
    db !TLL_ONLY
    db !BOTH_GAMES
    db !TLL_ONLY
    db !TLL_ONLY
    db !BOTH_GAMES
    db !BOTH_GAMES
    db !BOTH_GAMES

;----------------------------------------------------------------

;"WARP TO WORLD 000     "
LevelOptionText:
    dw $2c20, $2c0a, $2c1b, $2c19, $2c28, $2c1d, $2c18, $2c28
    dw $2020, $2018, $201b, $2015, $200d, $2028, $2000, $2000
    dw $2000, $2028, $2c28, $2c28, $2c28, $2c28, $2c28, $2c28
    dw $2c28, $2c28

DrawLevelOption:
    lda !MenuHardModeFlag   ;hard mode enabled?
    asl                     ;multiply by 2 for use as index
    tax
    beq +                   ;no, do not include star before world number
    lda #$2a                ;write star to buffer if hard mode is enabled
    sta !VRAM_BufferData+28
+:  lda !MenuWorldNumber    ;write world number
    inc                     ;increment for display, as usual
    sta !VRAM_BufferData+28,x
    lda #$24                ;write dash between world and level number
    sta !VRAM_BufferData+30,x
    lda !MenuLevelNumber    ;write level number
    inc                     ;increment for display, as usual
    sta !VRAM_BufferData+32,x
DrawOptionStub:             ;this is here if we don't need any buffer manipulation
    rts

AreaNumberPointers:
    dw AreaNumbers_SMB1,AreaNumbers_TLL
AreaNumbers_SMB1:
    db $00,$02,$03,$04 ; w1
    db $00,$02,$03,$04 ; w2
	db $00,$01,$02,$03 ; w3
	db $00,$02,$03,$04 ; w4
    db $00,$01,$02,$03 ; w5
    db $00,$01,$02,$03 ; w6
	db $00,$02,$03,$04 ; w7
	db $00,$01,$02,$03 ; w8
AreaNumbers_TLL:
	db $00,$02,$03,$04 ; w1
	db $00,$01,$02,$03 ; w2
	db $00,$02,$03,$04 ; w3
	db $00,$01,$02,$03 ; w4
	db $00,$02,$03,$04 ; w5
	db $00,$02,$03,$04 ; w6
	db $00,$01,$02,$03 ; w7
	db $00,$01,$02,$03 ; w8
	db $00,$01,$02,$03 ; w9
	db $00,$02,$03,$04 ; wA
	db $00,$02,$03,$04 ; wB
	db $00,$01,$02,$03 ; wC
	db $00,$01,$02,$03 ; wD
WarpToLevel:
    lda !MenuLevelNumber        ;store new level number
    sta !LevelNumber
    phx                         ;push X to stack for later
    txa                         ;note that game value is still in X
    asl
    tax                         ;multiply current game value by two
    rep #$20
    lda AreaNumberPointers,x    ;store pointer to table of area numbers
    sta $00
    sep #$20
    lda !MenuWorldNumber        ;store new world number
    sta !WorldNumber
    asl                         ;multiply by four and add level number for offset
    asl
    ora !LevelNumber
    tay
    lda ($00),y                 ;pull the correct area number and store it
    sta !AreaNumber
    cmp #$02                    ;is our area number $02?
    bne DisplayIntermediate     ;if not, always display intermediate lives screen
    dec                         ;decrement area number for comparison
    cmp !LevelNumber            ;if we're not on area number $02 and level 2,
    bne DisplayIntermediate     ;branch to display intermediate
    lda #$01                    ;otherwise, we'll set the disable intermediate flag
    bra DisableIntermediate     ;to prevent the lives screen from showing up
DisplayIntermediate:
    lda #$00                    ;load $00 to clear intermediate flag
DisableIntermediate:
    sta !SavedIntermediateFlag  ;store the flag here for later use
    plx                         ;pull game value from stack
    beq SetSMB1Stuff            ;if in SMB1, store hard mode flag appropiately
    lda #$00                    ;for TLL, clear these by default
    sta !CompletedWorlds
    sta !HardWorldsFlag
    sta !MoreDifficultQuestFlag
    ldy !WorldNumber            ;now comes complex conditionals for TLL's extra worlds
    cpy #!World9                ;did we select world 9+?
    bcc SetHardMode             ;if not, branch to clear hard mode
    inc !MoreDifficultQuestFlag ;if we're past world 8, $07fc is $01
    cpy #!World9                ;on world 9 specifically?
    beq SetHardMode             ;if so, branch to clear hard mode
    inc !HardWorldsFlag         ;this is set for worlds A through D
    lda !MenuHardModeFlag       ;finally, is the menu's hard mode flag set?
    beq SetHardMode             ;if not, clear hard mode for letter worlds (like the 8-4 -> A-1 transition)
    lda #$ff                    ;set $07fa to $ff, seems like a leftover from the FDS version...
    sta !CompletedWorlds
    lda #$01                    ;set hard mode for letter worlds (like the 9-4 -> A-1 transition)
    bra SetHardMode
SetSMB1Stuff:
    lda !MenuHardModeFlag       ;store hard mode flag in $07fc and $06fa for SMB1 levels
    sta !MoreDifficultQuestFlag
SetHardMode:
    sta !PrimaryHardMode
ForceLevelReload:               ;quick restart jumps here to preserve world/level numbers and such
    stz !OperMode_Task          ;reset modes of operation
    lda #!GameModeValue
    sta !OperMode
    sta !FetchNewGameTimerFlag  ;set flag to reset game timer
    sta !ScreenFadeoutFlag      ;set flag to do fadeout animation
    sta !FixFadeoutBGScroll     ;set flag to maintain scroll value of BG on fadeout (also needed to clear practice menu)
    stz !MosaicFadeoutFlag      ;do not perform mosaic fadeout
    stz !MoveSpritesOffscreen   ;clear flag responsible for moving sprites offscreen
    stz !AltEntranceControl     ;reset entrance type
    stz !JoypadOverride         ;reset joypad override to prevent vine from spawning
    stz !HalfwayPage            ;reset starting page to zero
    stz !GameTimerExpiredFlag   ;clear game timer expired flag to disable time-up screen
    stz !SkipMetatileBuffer     ;allow metatile buffer to update (needed if warping from 8-4/D-4 ending)
    stz !AreaMusicOverride      ;clear area music override address used by 8-4
    lda !SavedIntermediateFlag  ;copy saved intermediate flag to skip lives screen if needed
    sta !DisableIntermediate
    ldx #$06                    ;store RNG from practice menu in LFSR bytes
CopySavedRNG:
    lda !SavedRNGBytes,x        ;copy it over...
    sta !PseudoRandomBitReg,x
    dex
    bpl CopySavedRNG            ;...until we're all done
    rep #$20
    lda !SavedRNGNumber         ;copy saved RNG number over to current RNG number
    sta !CurrentRNGNumber       ;to maintain correct RNG-to-number parity
    sep #$20
    lda !SavedEntranceFrame     ;store entrance frame back in framerule timer
    sta !IntervalTimerControl   ;(required to correct interval timers)
    lda !SavedScreenFlag        ;invert old contents of $0e7f to use as indicator
    eor #%00000001              ;if we need to skip one RNG iteration for correct patterns
    sta !DelayRNGFlag           ;(this is related to title screen/end-of-castle not setting $0e7f on fade-out)
    stz !TimerControl           ;clear this to prevent an entrance desync from timers being paused
    ldx #$00                    ;set player size to big by default
    lda !SavedPlayerStatus      ;restore previously saved power-up status
    sta !PlayerStatus
    bne SetSize                 ;if we have a power-up, go ahead and set size
    inx                         ;otherwise increment X for small
SetSize:
    stx !PlayerSize             ;set size to maintain parity with power-up status
    lda !Saved1UpFlag           ;store flag to enable/disable hidden 1-UP mushroom
    sta !Hidden1UpFlag
    lda !SavedEntrancePage      ;copy over saved entrance page (relevant for wrong warps)
    sta !EntrancePage
    lda !SavedCoinTally         ;copy saved coin tally, relevant for fireworks in TLL
    sta !CoinTally
    jsr TwoDigitNumber          ;convert coin tally to two-digit decimal value
    sta !CoinDisplay+1          ;restore coin display used with status bar
    lda $00
    sta !CoinDisplay
    stz !LevelTimer             ;reset the level timer (necessary for underground/underwater stages)
    stz !LevelTimer+1
    stz !LevelTimer+2
    stz !LevelTimerFlags        ;disable operation of level timer and do not render on lives screen
    ldx !CurrentGame            ;use our current game to load the correct area pointers
    beq +
    jsl !LoadAreaPointer_TLL
    bra ++
+:  jsl !LoadAreaPointer_SMB1
++: %menu_confirm_sfx()         ;play confirmation sound
    jmp ForceMenuClose          ;force the menu to close to start the game loop

ControlLevelOption:
    ldx !CurrentGame        ;store smb1/tll flag in X register
    lda !JoypadBitsAPressed ;check buttons just pressed
    bpl CheckForLevelChange ;if B button not pressed, check for directional input
    jmp WarpToLevel         ;otherwise warp to the currently-selected level
CheckForLevelChange:
    pha                     ;push to stack for later
    lda !JoypadBitsAHeld    ;check buttons currently held
    asl
    bmi ModifyWorldNum      ;if holding Y, modify world number
    pla                     ;check directions just pressed
    bit #%00000110
    bne DecrementLevel      ;if pressing down or left, decrement level
    bit #%00001001
    bne IncrementLevel      ;if pressing up or right, increment level
    rts                     ;if pressing neither, leave
ModifyWorldNum:
    pla                     ;check directions just pressed
    bit #%00000110
    bne DecWorldNum         ;if pressing down or left, decrement world
    bit #%00001001
    bne IncWorldNum         ;if pressing up or right, increment world
    rts                     ;if pressing neither, leave
DecWorldNum:
    jsr DecrementWorld      ;decrement world number
    bra RedrawLevelOption
IncWorldNum:
    jsr IncrementWorld      ;increment world number
    bra RedrawLevelOption
DecrementLevel:
    lda !MenuLevelNumber    ;decrement level number
    dec
    bpl StoreLevelNumber    ;if in range, store level number
    jsr DecrementWorld      ;otherwise decrement world number
    lda #!Level4            ;then wrap level number back around
StoreLevelNumber:
    sta !MenuLevelNumber
RedrawLevelOption:
    inc !DrawOptionFlag     ;redraw menu option
    %edit_value_sfx()
    rts
IncrementLevel:
    lda !MenuLevelNumber    ;increment menu option
    inc
    cmp #!Level4+1          ;past level 4?
    bcc StoreLevelNumber    ;no, store the level number
    jsr IncrementWorld      ;otherwise increment world number
    lda #!Level1            ;and force level number back to 1
    bra StoreLevelNumber
;this code is probably needlessly convoluted but oh well
DecrementWorld:
    lda !MenuWorldNumber    ;subtract one from world number
    dec
    cmp #!World9            ;is the world number less than 9?
    bcc StoreWorldNumber    ;world number is between 1 and 8, store it
    cpx #$00                ;check current game if world number >= 9
    beq DecWorld_SMB1       ;if SMB1, must have underflowed, invert hard mode
    cmp #$00                ;if the world number underflowed in TLL,
    bmi DecWorld_TLL        ;set it to D with hard mode enabled
    cmp #!WorldA            ;if we're between worlds A through D,
    bcs StoreWorldNumber    ;we're good to store the world number
    ldy !MenuHardModeFlag   ;if world number = 9, are we on hard mode?
    beq StoreWorldNumber    ;no, go from world A to world 9
    dec !MenuHardModeFlag   ;otherwise go from hard mode A to no hard mode D
    bra ForceWorldD
DecWorld_TLL:
    inc !MenuHardModeFlag   ;set hard mode for letter worlds
ForceWorldD:
    lda #!WorldD            ;set level select for world D
    bra StoreWorldNumber
DecWorld_SMB1:
    lda !MenuHardModeFlag   ;invert hard mode flag
    eor #%00000001
    sta !MenuHardModeFlag
    lda #!World8            ;force world 8 to wrap around
StoreWorldNumber:
    sta !MenuWorldNumber    ;store world number
    rts
IncrementWorld:
    lda !MenuWorldNumber    ;add one to world number
    inc
    cmp #!World9            ;are we in worlds 1 through 8?
    bcc StoreWorldNumber    ;if so, we're good to store world number
    cpx #$00                ;check current game
    beq InvertHardModeW1    ;if SMB1, must have overflowed, invert hard mode
    cmp #!WorldD+1          ;past world D in TLL?
    bcc StoreWorldNumber    ;nope, store the world number
    lda !MenuHardModeFlag   ;are we past hard mode world D?
    bne InvertHardModeW1    ;yes, so invert hard mode and go back to world 1
    inc !MenuHardModeFlag   ;otherwise set hard mode letter worlds
    lda #!WorldA            ;set for world A
    bra StoreWorldNumber
InvertHardModeW1:
    lda !MenuHardModeFlag   ;invert hard mode flag here
    eor #%00000001
    sta !MenuHardModeFlag
    lda #!World1            ;force world 1 to wrap around
    bra StoreWorldNumber

;----------------------------------------------------------------

;"ENTRANCE = 0000 0 0       "
EntranceOptionText:
    dw $2c0e, $2c17, $2c1d, $2c1b, $2c0a, $2c17, $2c0c, $2c0e
    dw $2c28, $2c5e, $2c28, $2c00, $2c00, $2c00, $2c00, $2c28
    dw $2c00, $2c28, $2c00, $2c28, $2c28, $2c28, $2c28, $2c28
    dw $2c28, $2c28

DrawEntranceOption:
    lda !MenuSelectionIndex     ;use selection index to color selection
    asl                         ;multiply selection by 4
    asl
    tax                         ;put in X as index
    cpx #$08                    ;if selection 2 or 3, branch
    bcs OneDigitSel
    lda #$20                    ;otherwise color the byte selected
    sta !VRAM_BufferData+23,x
    sta !VRAM_BufferData+25,x
    bra EntranceStuff
OneDigitSel:
    lda #$20                    ;use index to color the correct single-digit option
    sta !VRAM_BufferData+25,x
EntranceStuff:
    lda !MenuRNGNumber          ;convert menu RNG number to nybbles and write to buffer
    jsr GetNybbles
    sta !VRAM_BufferData+28
    tya
    sta !VRAM_BufferData+26
    lda !MenuRNGNumber+1
    jsr GetNybbles
    sta !VRAM_BufferData+24
    tya
    sta !VRAM_BufferData+22
    lda !MenuEntranceFrame      ;take menu entrance frame and subtract 5 for display
    sec
    sbc #5
    bpl StoreEntranceFrame
    clc
    adc #21                     ;wrap around if needed
StoreEntranceFrame:
    sta !VRAM_BufferData+32
    lda !MenuScreenFlag         ;write menu screen flag to buffer
    sta !VRAM_BufferData+36
    rts

AddSubFramerule:
    txa                     ;perform directional pad checks
    bit #%00001000
    bne AddFramerule        ;if pressing up, advance to next framerule
    bit #%00000100
    bne SubFramerule        ;if pressing down, go back to previous framerule
    rts
SubFramerule:
    rep #$20                ;set 16-bit accumulator
    lda !MenuRNGNumber      ;subtract 21 ($15) from RNG number for previous framerule
    sec
    sbc #$0015
    dec                     ;decrement to account for case where result is $0000
    bpl IncUpdateFramerule  ;if RNG number within range, undo decrement to store correct number
    bmi UpdateFramerule     ;if not, branch ahead to get number back in range and store
AddFramerule:
    rep #$20                ;set 16-bit accumulator
    lda !MenuRNGNumber      ;add 21 ($15) to RNG number for next framerule
    clc
    adc #$0015
    bpl UpdateFramerule     ;if RNG number hasn't gone past $7fff, branch ahead
IncUpdateFramerule:
    inc                     ;otherwise increment RNG number since we start at $0001
UpdateFramerule:
    and #$7fff              ;mask out d15 to keep RNG number within range
    sta !MenuRNGNumber      ;store the new RNG number
    sep #$20                ;restore 8-bit accumulator
    %edit_value_sfx()       ;play edit value sound effect
    inc !DrawOptionFlag     ;set flag to redraw menu and leave
    rts
HandleRNGNumber:
    lda !JoypadBitsBHeld    ;check if we're holding the X button
    asl
    bmi AddSubFramerule     ;if we are, add or subtract RNG by 21 like framerules
    ldx #$ff                ;byte's bitmask is $ff for low byte by default
    lda !MenuSelectionIndex ;store option in A
    bne +                   ;if doing low byte, branch
    ldx #$7f                ;bitmask is now $7f for high byte
+:  stx $02
    eor #%00000001          ;invert selection so high byte is before low byte
    tax                     ;transfer to X to use for calculation
    rep #$20
    lda #$0000              ;clear out high byte for address
    txa
    clc
    adc #!MenuRNGNumber     ;add base address plus X to get correct byte location
    sta $00                 ;store in $00 to use as pointer
    jmp HandleHexNumInput   ;go to general routine for hex number input
ControlEntranceOption:
    ldx !JoypadBitsAPressed ;load joypad bits pressed into X register
    bmi UpdateRNGValues     ;if we pressed B to confirm, branch to update RNG
    lda !JoypadBitsBPressed ;check if the A button has been pressed
    bpl +                   ;if not, branch ahead for no confirmation
    stz !MenuRNGNumber      ;otherwise, clear menu RNG number
    stz !MenuRNGNumber+1    ;to activate the "random" setting
UpdateRNGValues:
    %menu_confirm_sfx()     ;otherwise play confirm sfx
    inc !DrawOptionFlag     ;set flag to redraw option
    jmp SetRNGFromNumber    ;and save our new RNG/entrance values
+:  txa                     ;now perform directional pad checks
    bit #%00000001
    bne IncEntranceSel      ;if pressing right, increment entrance selection
    bit #%00000010
    bne DecEntranceSel      ;if pressing left, decrement entrance selection
    ldy !MenuSelectionIndex ;get menu selection index
    beq HandleRNGNumber     ;do RNG number high byte if zero
    dey
    beq HandleRNGNumber     ;do RNG number low byte if one
    dey
    beq HandleEntranceFrame ;do entrance frame (i.e. framerule timer) if two
    bit #%00001100          ;otherwise we're doing the screen flag
    bne InvertScreenFlag    ;if pressing up or down, invert screen flag
    rts
IncEntranceSel:
    %increment_option(!MenuSelectionIndex,#4)
DecEntranceSel:
    %decrement_option(!MenuSelectionIndex,#4)
HandleEntranceFrame:
    bit #%00000100
    bne DecrementEntrance   ;if pressing down, increment entrance frame
    bit #%00001000
    bne IncrementEntrance   ;if pressing up, increment entrance frame
    rts
DecrementEntrance:
    %decrement_option(!MenuEntranceFrame,#21)
IncrementEntrance:
    %increment_option(!MenuEntranceFrame,#21)
InvertScreenFlag:
    %invert_option(!MenuScreenFlag)

;----------------------------------------------------------------

;"RAM ADDRESSES = 0000 0000 "
AddressOptionText:
    dw $2c1b, $2c0a, $2c16, $2c28, $2c0a, $2c0d, $2c0d, $2c1b
    dw $2c0e, $2c1c, $2c1c, $2c0e, $2c1c, $2c28, $2c5e, $2c28
    dw $2c00, $2c00, $2c00, $2c00, $2c28, $2c00, $2c00, $2c00
    dw $2c00, $2c28

RAMPalleteLookupTable:
    db $00,$04,$0a,$0e
DrawAddressesOption:
    ldx !MenuSelectionIndex     ;color selected option using X
    lda RAMPalleteLookupTable,x ;refer to lookup table to get the index
    tax
    lda #$20                    ;color the selected byte
    sta !VRAM_BufferData+33,x
    sta !VRAM_BufferData+35,x
DrawRAMAddrs:
    lda !CustomAddressB         ;draw custom address B first
    jsr GetNybbles
    sta !VRAM_BufferData+48
    tya
    sta !VRAM_BufferData+46
    lda !CustomAddressB+1       ;high byte of custom address B
    jsr GetNybbles
    sta !VRAM_BufferData+44
    tya
    sta !VRAM_BufferData+42
    lda !CustomAddressA         ;then draw custom address A
    jsr GetNybbles
    sta !VRAM_BufferData+38
    tya
    sta !VRAM_BufferData+36
    lda !CustomAddressA+1       ;high byte of custom address A
    jsr GetNybbles
    sta !VRAM_BufferData+34
    tya
    sta !VRAM_BufferData+32
    rts

ControlAddressesOption:
    lda !JoypadBitsAPressed ;now check directions just pressed
    bit #%00000001
    bne AddByteSelect       ;if pressing right, select other address
    bit #%00000010
    bne SubByteSelect       ;if pressing left, select other address
    ldx #$ff                ;bitmask is $ff for low byte by default
    lda !MenuSelectionIndex ;store selected address byte in A
    pha                     ;push to stack for later
    lsr
    bcs +                   ;if we're doing the low byte, branch
    ldx #$1f                ;otherwise bitmask is now $1f for high byte
+:  stx $02                 ;store the bitmask
    pla                     ;retrieve selection from stack
    eor #%00000001          ;invert so we do high byte before low byte
    tax
    rep #$20
    lda #$0000              ;have to clear out high byte
    txa                     ;add selection to base address for correct pointer
    clc
    adc #!CustomAddressA
    sta $00                 ;store pointer and go to routine for general hex input
    jmp HandleHexNumInput
AddByteSelect:
    %increment_option(!MenuSelectionIndex,#4)
SubByteSelect:
    %decrement_option(!MenuSelectionIndex,#4)

;----------------------------------------------------------------

;"POWER-UP STATE = 00000    "
PowerupOptionText:
    dw $2c19, $2c18, $2c20, $2c0e, $2c1b, $2c24, $2c1e, $2c19
    dw $2c28, $2c1c, $2c1d, $2c0a, $2c1d, $2c0e, $2c28, $2c5e
    dw $2c28, $2000, $2000, $2000, $2000, $2000, $2c28, $2c28
    dw $2c28, $2c28

PowerupPointers:
    dw Small, Super, Fire

Small:
    dw $201c, $2016, $200a, $2015, $2015 ;"SMALL"
Super:
    dw $201c, $201e, $2019, $200e, $201b ;"SUPER"
Fire:
    dw $200f, $2012, $201b, $200e, $2028 ;"FIRE "

DrawPowerupOption:
    lda !MenuPlayerStatus       ;take player's status selection
    asl                         ;multiply by two for use as pointer
    tax
    rep #$20
    lda PowerupPointers,x       ;store pointer to correct status text
    sta $00
    sep #$20
    ldy #$08                    ;do loop to write text to buffer
-:  lda ($00),y
    sta !VRAM_BufferData+34,y
    dey
    dey
    bpl -
    rts

ControlPowerupOption:
    lda !JoypadBitsAPressed ;check buttons just pressed
    bmi UpdatePowerup       ;if B button pressed, update our powerup state
    bit #%00000110
    bne DecrementPowerup    ;if pressing down or left, decrement powerup state
    bit #%00001001
    bne IncrementPowerup    ;if pressing up or right, increment powerup state
    rts                     ;if pressing neither, leave
UpdatePowerup:
    ldy #$00                ;set player size as big by default
    %powerup_sfx()          ;play power-up sound by default
    lda !MenuPlayerStatus   ;store menu powerup as actual powerup
    sta !PlayerStatus
    sta !SavedPlayerStatus  ;also update saved copy for level reload
    bne +                   ;if not small, branch
    iny                     ;otherwise increment for correct size
    %pipe_sfx()             ;play damage sound instead of power-up sound
    stz $1603               ;remove power-up sound from queue
+:  sty !PlayerSize         ;set player size
    rts                     ;leave
IncrementPowerup:
    %increment_option(!MenuPlayerStatus,#3)
DecrementPowerup:
    %decrement_option(!MenuPlayerStatus,#3)

;----------------------------------------------------------------

;"HIDDEN 1-UP BLOCK 00000000"
Hidden1UpOptionText:
    dw $2c11, $2c12, $2c0d, $2c0d, $2c0e, $2c17, $2c28, $2c01
    dw $2c24, $2c1e, $2c19, $2c28, $2c0b, $2c15, $2c18, $2c0c
    dw $2c14, $2c28, $2000, $2000, $2000, $2000, $2000, $2000
    dw $2000, $2000

EnabledDisabledPointers:
    dw Disabled,Enabled

Disabled:
    dw $200d,$2012,$201c,$200a,$200b,$2015,$200e,$200d ;"DISABLED"
Enabled:
    dw $200e,$2017,$200a,$200b,$2015,$200e,$200d,$2028 ;"DISABLED"

Draw1UpOption:
    lda !Menu1UpFlag                ;get menu 1-UP flag for use as offset
    asl                             ;multiply by two for 16-bit words
    tax
    rep #$20
    lda EnabledDisabledPointers,x   ;load appropiate pointer into $00
    sta $00
    sep #$20
    ldy #$0e                        ;do loop to write text to menu buffer
-:  lda ($00),y
    sta !VRAM_BufferData+36,y
    dey
    dey
    bpl -
    rts

Control1UpOption:
    lda !JoypadBitsAPressed ;check buttons just pressed
    bmi Update1UpFlag       ;if B button pressed, update saved 1-UP flag
    bit #%00001111
    bne Invert1UpFlag       ;if pressing any direction, invert 1-UP flag
    rts                     ;if pressing neither, leave
Invert1UpFlag:
    %invert_option(!Menu1UpFlag)
Update1UpFlag:
    lda !Menu1UpFlag        ;save menu's 1-UP flag into saved copy
    sta !Saved1UpFlag       ;(used for level reload/warp feature)
    sta !Hidden1UpFlag      ;also change the current 1-UP flag
    %menu_confirm_sfx()     ;play confirmation sound
    rts                     ;leave

;----------------------------------------------------------------

;"GRANT INVINCIBILITY       "
InvincibilityOptionText:
    dw $2010, $201b, $200a, $2017, $201d, $2028, $2012, $2017
    dw $201f, $2012, $2017, $200c, $2012, $200b, $2012, $2015
    dw $2012, $201d, $2022, $2c28, $2c28, $2c28, $2c28, $2c28
    dw $2c28, $2c28

;for now, this works like Pellsson's "GIVE STAR" feature...
;perhaps in the future, we can change this to toggleable intangibility?
ControlInvincibilityOption:
    lda !JoypadBitsAPressed ;check buttons just pressed
    bpl +                   ;if B button not pressed, leave
    lda #$ff                ;otherwise give invincibility
    sta !StarInvincibleTimer
    %powerup_sfx()          ;play power-up sound
+:  rts

;----------------------------------------------------------------

;"RETURN TO TITLE SCREEN    "
TitleOptionText:
    dw $201b, $200e, $201d, $201e, $201b, $2017, $2028, $201d
    dw $2018, $2028, $201d, $2012, $201d, $2015, $200e, $2028
    dw $201c, $200c, $201b, $200e, $200e, $2017, $2c28, $2c28
    dw $2c28, $2c28

ControlTitleOption:
    lda !JoypadBitsAPressed     ;check buttons just pressed
    bpl +                       ;if B button not pressed, leave
    stz !OperMode               ;reset modes of operation
    stz !OperMode_Task          ;to return player to title screen
    stz !MoveSpritesOffscreen   ;clear flag related to sprites
    stz !SkipMetatileBuffer     ;allow metatile buffer to update
    stz !CurrentRNGNumber       ;reset RNG number too, just in case
    stz !CurrentRNGNumber+1
    stz !CurrentBrother         ;smb1 requires this so we don't have red luigi
    ;stz !HardWorldsFlag         ;clear flag to play normal smb1 demo inputs
    ;stz !MoreDifficultQuestFlag ;disable hard mode to prevent smb1 demo desync
    stz !MosaicFadeoutFlag      ;do not perform mosaic fadeout
    lda #$01
    sta !FixFadeoutBGScroll     ;set flag to perform fade-out
    ;lda #$ff
    ;sta !DemoEnable_TLL         ;re-enable the demo for tll
    inc !DisableScreenFlag      ;set screen disable flag
    %menu_confirm_sfx()         ;play confirmation sound
    %fadeout_music()            ;set music fade-out
    jmp ForceMenuClose          ;force the practice menu to close
+:  rts

;----------------------------------------------------------------

;"CURRENT PLAYER = 00000    "
PlayerOptionText:
    dw $2c0c, $2c1e, $2c1b, $2c1b, $2c0e, $2c17, $2c1d, $2c28
    dw $2c19, $2c15, $2c0a, $2c22, $2c0e, $2c1b, $2c28, $2c5e
    dw $2c28, $2000, $2000, $2000, $2000, $2000, $2c28, $2c28
    dw $2c28, $2c28

PlayerPointers:
    dw Mario, Luigi

Mario:
    dw $2016, $200a, $201b, $2012, $2018 ;"MARIO"
Luigi:
    dw $2015, $201e, $2012, $2010, $2012 ;"LUIGI"

DrawPlayerOption:
    lda !MenuSelectedPlayer     ;get selected player as offset
    asl                         ;multiply by two since we're dealing with words
    tax
    rep #$20
    lda PlayerPointers,x        ;load appropiate pointer into $00
    sta $00
    sep #$20
    ldy #$08                    ;do loop to write player name to menu buffer
-:  lda ($00),y
    sta !VRAM_BufferData+34,y
    dey
    dey
    bpl -
    rts

ControlPlayerOption:
    lda !JoypadBitsAPressed ;check buttons just pressed
    bmi UpdatePlayer        ;if B button pressed, update current player
    bit #%00001111
    bne InvertPlayer        ;if pressing any direction, invert player
    rts                     ;if pressing neither, leave
InvertPlayer:
    %invert_option(!MenuSelectedPlayer)
UpdatePlayer:
    lda !MenuSelectedPlayer ;save current player in both player-related variables
    sta !SelectedPlayer
    sta !CurrentBrother
    %menu_confirm_sfx()     ;play confirmation sound
    ;update status bar
    rep #$20                ;enable 16-bit registers
    lda #$4358              ;location of text, LSB then MSB
    sta !VRAM_BufferAddr
    lda #$0900              ;length of text, LSB then MSB
    sta !VRAM_BufferLen
    lda !SelectedPlayer     ;fetch current player for use as offset
    asl
    tax
    lda PlayerPointers,x    ;store offset in temp RAM
    sta $00
    sep #$20
    ldy #$09                ;do loop to write player name to status bar
-:  lda ($00),y
    sta !VRAM_BufferData,y
    dey
    bpl -
    sep #$30                ;disable 16-bit registers
    lda #$ff                ;apply terminator to end of VRAM buffer
    sta !VRAM_BufferData+10
    lda #15
    sta !VRAM_BufferOffset  ;update VRAM buffer offset accordingly
    rts

;----------------------------------------------------------------

;"COIN COUNT = 00           "
CoinsOptionText:
    dw $2c0c, $2c18, $2c12, $2c17, $2c28, $2c0c, $2c18, $2c1e
    dw $2c17, $2c1d, $2c28, $2c5e, $2c28, $2000, $2000, $2c28
    dw $2c28, $2c28, $2c28, $2c28, $2c28, $2c28, $2c28, $2c28
    dw $2c28, $2c28

DrawCoinsOption:
    lda !MenuCoinTally      ;write menu coin tally as two-digit number
    jsr TwoDigitNumber
    sta !VRAM_BufferData+28
    lda $00
    sta !VRAM_BufferData+26
    rts

ControlCoinsOption:
    lda !JoypadBitsAPressed ;check buttons just pressed
    bmi UpdateCoins         ;if B button pressed, update coin count
    pha                     ;retrieve pressed buttons later
    lda !JoypadBitsAHeld    ;check held buttons
    asl
    bmi CoinTens            ;if holding Y, do tens place
    pla
    bit #%00000110
    bne DecrementCoins      ;if pressing down or left, decrement coins
    bit #%00001001
    bne IncrementCoins      ;if pressing up or right, increment coins
    rts
UpdateCoins:
    lda !MenuCoinTally     ;update internal coin tally
    sta !CoinTally
    sta !SavedCoinTally    ;update saved copy of coin tally
    jsr TwoDigitNumber     ;perform sub to convert to decimal
    sta !CoinDisplay+1     ;update coin display used with status bar
    lda $00
    sta !CoinDisplay
    %coin_sfx()
    ;update status bar
    rep #$20                ;enable 16-bit A
    lda #$6d58              ;location of text, LSB then MSB
    sta !VRAM_BufferAddr
    lda #$0300              ;length of text, LSB then MSB
    sta !VRAM_BufferLen
    sep #$20                ;disable 16-bit A
    lda #$20                ;set palette and priority
    sta !VRAM_BufferData+1
    sta !VRAM_BufferData+3
    lda !CoinDisplay        ;write coin display to status bar immediately
    sta !VRAM_BufferData
    lda !CoinDisplay+1
    sta !VRAM_BufferData+2
    lda #$ff                ;apply terminator to end of VRAM buffer
    sta !VRAM_BufferData+4
    lda #8
    sta !VRAM_BufferOffset  ;update VRAM buffer offset accordingly
    rts                     ;leave
IncrementCoins:
    %increment_option(!MenuCoinTally,#100)
DecrementCoins:
    %decrement_option(!MenuCoinTally,#100)
CoinTens:
    pla
    bit #%00000110
    bne SubTenCoins      ;if pressing down or left, decrement tens place
    bit #%00001001
    bne AddTenCoins      ;if pressing up or right, increment tens place
    rts
SubTenCoins:
    lda !MenuCoinTally   ;decrement tens place of menu coin tally
    sec
    sbc #10
    bpl ChangeCoinTally  ;if we didn't go below zero, branch to store
    clc
    adc #100             ;otherwise add 100 to fix coin tally
    bra ChangeCoinTally
AddTenCoins:
    lda !MenuCoinTally   ;increment tens place of menu coin tally
    clc
    adc #10
    cmp #100
    bcc ChangeCoinTally  ;if not past 99, branch to store
    sbc #100             ;otherwise subtract 100 to fix coin tally
    bra ChangeCoinTally  ;(we know carry is set here)
ChangeCoinTally:
    sta !MenuCoinTally   ;store new coin tally
    %edit_value_sfx()
    inc !DrawOptionFlag  ;redraw option and leave
    rts

;----------------------------------------------------------------

;"WARPS USED = 000          "
WarpsOptionText:
    dw $2c20, $2c0a, $2c1b, $2c19, $2c1c, $2c28, $2c1e, $2c1c
    dw $2c0e, $2c0d, $2c28, $2c5e, $2c28, $2000, $2000, $2000
    dw $2c28, $2c28, $2c28, $2c28, $2c28, $2c28, $2c28, $2c28
    dw $2c28, $2c28

YesNoPointers:
    dw No,Yes

No:
    dw $2017,$2018,$2028 ;"NO "
Yes:
    dw $2022,$200e,$201c ;"YES"

DrawWarpsOption:
    lda !MenuWarpsFlag  ;use value of flag as offset
    asl                 ;multiply by two for words
    tax
    rep #$20
    lda YesNoPointers,x ;store pointer to correct words
    sta $00
    sep #$20
    ldy #$04            ;init counter for loop
-:  lda ($00),y         ;copy text to buffer
    sta !VRAM_BufferData+26,y
    dey
    bpl -
    rts

ControlWarpsOption:
    lda !JoypadBitsAPressed ;check buttons just pressed
    bmi UpdateWarpsFlag     ;if B button pressed, update warps flag
    bit #%00001111
    bne InvertWarpsFlag     ;if pressing any direction, invert flag
    rts                     ;if pressing neither, leave
InvertWarpsFlag:
    %invert_option(!MenuWarpsFlag)
UpdateWarpsFlag:
    lda !MenuWarpsFlag      ;copy menu flag to $0f2b
    sta !WarpsUsedFlag
    %menu_confirm_sfx()
    rts

;----------------------------------------------------------------
;general-use routines

;sub to convert byte to two/three digit number (uses $00 and $01)
ThreeDigitNumber:
            stz $01 ;hundreds in $01
Hundreds:   cmp #100
            bcc TwoDigitNumber
            sbc #100
            inc $01
            bra Hundreds
TwoDigitNumber:
            stz $00 ;tens in $00
Tens:       cmp #10
            bcc Ones
            sbc #10
            inc $00
            bra Tens
Ones:       rts ;ones in A

;subroutine to seperate both nybbles of a byte, uses Y register
GetNybbles:
    pha ;push full byte to stack
    lsr
    lsr
    lsr
    lsr
    tay ;store high nybble in Y
    pla ;pull full byte and return low nybble in A
    and #$0f
    rts

;$00-$01 = pointer to four digit hex value, $02 = bitmask
PowersOfSixteenTable:
    db $01,$10
HandleHexNumInput:
    sep #$20                ;restore 8-bit accumulator
    ldx #$00                ;set index for LUT to zero by default
    lda !JoypadBitsAHeld
    bit #%01000000          ;holding Y button?
    beq CheckAddSubValue    ;no, branch
    inx                     ;if holding Y, increment index for high nybble
CheckAddSubValue:
    lda !JoypadBitsAPressed ;now check directions just pressed
    bit #%00000100
    bne SubtractValue       ;if pressing down, subtract from value
    bit #%00001000
    bne AddValue            ;if pressing up, add to value
    rts                     ;if pressing neither, leave
SubtractValue:
    lda ($00)               ;subtract one from correct nybble
    sec
    sbc PowersOfSixteenTable,x
    bra MaskValue
AddValue:
    lda ($00)               ;add one to correct nybble
    clc
    adc PowersOfSixteenTable,x
MaskValue:
    and $02                 ;mask byte according to bitmask in $02
    sta ($00)               ;store the resulting value at the address $00-$01 points to
    %edit_value_sfx()
    inc !DrawOptionFlag     ;set flag to redraw option
    rts

;$00-$01 = pointer to option, $02 = number of options
IncrementOption:
    lda ($00)           ;increment option given by pointer
    inc
    cmp $02             ;are we past the last option?
    bcc +               ;no, store option number
    lda #$00            ;otherwise wrap around to first option
    bra +
DecrementOption:
    lda ($00)           ;decrement option given by pointer
    dec
    bpl +               ;store if we didn't go past first option
    lda $02             ;otherwise take number of options
    dec                 ;subtract one and store for last option
+:  sta ($00)           ;store option number at given address
    %edit_value_sfx()
    inc !DrawOptionFlag ;redraw whatever option we're on
    rts

InvertOption:
    lda ($00)               ;invert option given by pointer
    eor #%00000001
    sta ($00)
    %edit_value_sfx()
    inc !DrawOptionFlag     ;redraw menu option
    rts

;----------------------------------------------------------------

;custom status bar tiles
org $0cfde0
    db $00,$00,$7E,$7E,$FF,$81,$7E,$7E  ;equal sign
    db $7E,$7E,$FF,$81,$7E,$7E,$00,$00
    db $00,$00,$7C,$7C,$FE,$82,$FC,$84  ;flag icon for smb1
    db $F8,$88,$F0,$90,$E0,$A0,$40,$40
    db $0C,$0C,$1E,$12,$3E,$22,$7E,$42  ;cursor icon for menu
    db $7E,$42,$3E,$22,$1E,$12,$0C,$0C
    db $20,$20,$70,$50,$70,$50,$20,$20  ;prime
    db $00,$00,$00,$00,$00,$00,$00,$00
    db $28,$28,$7C,$54,$7C,$54,$28,$28  ;double prime
    db $00,$00,$00,$00,$00,$00,$00,$00
