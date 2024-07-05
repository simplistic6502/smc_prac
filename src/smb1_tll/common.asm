;don't increment lives
org $048596
    stz !NumberOfLives

org $05ee5d ;we stuff all practice functions at the end of bank $05 for now
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
    %update_buffer_offset($0001)    ;apprend terminator and update buffer offset
    pla                             ;pull from stack to set Z flag, if relevant
    rtl                             ;return

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
UpdateFrameCounterInner:            ;remainder update always updates frame counter too!
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
    lda !Enemy_X_Position+9             ;perform the calculation that causes backwards flag grab
    sec
    sbc !ScreenEdge_X_Pos
    jsr GetNybbles                      ;get nybbles for result
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

UpdateLagCounter:
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
++: rtl

ChgAreaModeHijack:              ;hijack to store game info on warp zone completion
    lda !WarpZoneControl        ;did we exit level via warpzone?
    beq +                       ;skip custom subroutine if not
    jsr SaveRNG_EntranceFrame   ;otherwise we took a warp zone and want to save info
+:  lda #$00                    ;set operation task and leave
    sta !OperMode_Task
    rtl
LoadAreaPointerHijack_SMB1:
    ;lda !DisableIntermediate    ;is the disable intermediate flag set?
    ;bne +                       ;if so, skip routine to save RNG/entrance state
    jsr SaveRNG_EntranceFrame   ;save RNG and other relevant info for stage reload
    jml !LoadAreaPointer_SMB1   ;load area pointer and don't return here
LoadAreaPointerHijack_TLL:
    ;lda !DisableIntermediate    ;is the disable intermediate flag set?
    ;bne +                       ;if so, skip routine to save RNG/entrance state
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
    lda !DisableIntermediate    ;finally, save flag used to disable intermediate
    sta !SavedIntermediateFlag  ;if we need to for a x-2 level
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
    jml !SoundEngine            ;go handle the sound engine now

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
    beq DoneWithRNG             ;if so, don't bother doing any iteration
AdvanceRNGLoop:
    jsr AdvanceRNG              ;otherwise iterate through RNG until the counter hits zero
    dex
    bne AdvanceRNGLoop
DoneWithRNG:
    sep #$30
    lda !MenuEntranceFrame      ;copy other entrance values from menu to saved copies
    sta !SavedEntranceFrame
    lda !MenuScreenFlag
    sta !SavedScreenFlag
    rts

;RNG algorithm as taken from the game itself
AdvanceRNG:
    lda !SavedRNGBytes     ;XOR current d49 and d41
    and #%00000010
    sta $00
    lda !SavedRNGBytes+1
    and #%00000010
    eor $00
    clc                    ;clear or set carry based on result
    beq +
    sec
+:  ror !SavedRNGBytes     ;shift all bits to the right,
    ror !SavedRNGBytes+1   ;discard the current d0, and set
    ror !SavedRNGBytes+2   ;the new d55 based on the earlier XOR
    ror !SavedRNGBytes+3
    ror !SavedRNGBytes+4
    ror !SavedRNGBytes+5
    ror !SavedRNGBytes+6
    rts

UpdateCustomAddresses:
    ldx !VRAM_BufferOffset   ;only do this if VRAM buffer is empty (to limit buffer size)
    bne +
    ldx !PauseModeFlag       ;skip over this if pause menu is open because there's no point in updating
    bne +
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

PracticeMenu_SMB1:
    jsl !ReadJoypads_SMB1       ;read controllers
    stz !CurrentGame            ;use variable to indicate we're in smb1
    jsr HandlePracticeMenu      ;do practice menu stuff
    jsr UpdateCustomAddresses   ;update the custom address values
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
    jsr HandlePracticeMenu      ;do practice menu stuff
    jsr UpdateCustomAddresses   ;update the custom address values
    lda !PracticeMenuFlag       ;if we have the menu enabled, only run audio subs
    bne SkipGameLoop_TLL
    lda !DelayRNGFlag           ;do we have to delay RNG because of a stupid quirk?
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
    lda !JoypadBits1Pressed     ;after all this, check if we're pressing select to open the menu
    and #%00100000
    cmp #%00100000
    beq PreparePracticeMenu     ;if we are, prepare the menu
    lda !JoypadBits2Held        ;but if we're not, check for quick restart combo,
    and #%00110000              ;which uses the L+R shoulder buttons
    cmp #%00110000
    bne ExitMenuCheck           ;if we aren't holding those buttons, leave
    lda !FixFadeoutBGScroll     ;otherwise check if we're doing a fadeout or fadein
    bne ExitMenuCheck           ;yes, leave
    jmp ForceLevelReload        ;no, restart the current level with RNG and entrance frame intact
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
    stz !MenuSelectionIndex     ;clear menu selection index
    %menu_open_sfx()            ;play sfx for opening menu
    bra RunPracticeMenu         ;run the practice menu code to immediately draw option

CheckForMenuClose:
    lda !JoypadBits1Pressed     ;check if we're pressing select to close the menu
    and #%00100000
    cmp #%00100000
    bne RunPracticeMenu         ;if we aren't, run the menu
    %menu_close_sfx()           ;play sfx if we're closing the menu
ForceMenuClose:
    stz !PracticeMenuFlag       ;clear practice menu flag
    %setup_vram_buffer($2358,$3340)
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
    dw EntranceOptionText
    dw RNGOptionText
    dw RAMOptionText
    dw PowerupOptionText
    dw InvincibilityOptionText
    dw PlayerOptionText
    dw CoinsOptionText
    dw WarpsOptionText
OptionDrawPointers:
    dw DrawLevelOption
    dw DrawEntranceOption
    dw DrawRNGOption
    dw DrawAddressesOption
    dw DrawPowerupOption
    dw DrawOptionStub
    dw DrawPlayerOption
    dw DrawCoinsOption
    dw DrawWarpsOption

MaxOptions:
    db !MAX_OPTIONS_SMB1,!MAX_OPTIONS_TLL
ControlPracticeMenu:
    ldy !CurrentGame            ;store the current game in Y
    lda !JoypadBits2Pressed     ;check if L or R were pressed
    bit #%00010000
    bne RPress                  ;go here if R was pressed
    bit #%00100000
    bne LPress                  ;go here if L was pressed
    bra ControlOption           ;otherwise control selected option
RPress:
    lda !PracticeMenuOption     ;increment to next option
    inc
    cmp MaxOptions,y            ;are we past the max option allowed?
    bcc +                       ;no, set option number
    lda #$00                    ;yes, wrap option number around to zero
    bra +
LPress:
    lda !PracticeMenuOption     ;decrement to previous option
    dec
    bpl +                       ;are we past the first option?
    lda MaxOptions,y            ;if we are, get the max number of options
    dec                         ;and decrement to go to last menu option
+:  sta !PracticeMenuOption     ;store our menu option here
    %change_option_sfx()
    inc !DrawOptionFlag
    stz !MenuSelectionIndex     ;clear selection index within option
    rts                         ;leave
ControlOption:
    lda !PracticeMenuOption     ;control whatever option we're currently on
    asl
    tax
    jmp (OptionCtrlPointers,x)
OptionCtrlPointers:
    dw ControlLevelOption
    dw ControlEntranceOption
    dw ControlRNGOption
    dw ControlAddressesOption
    dw ControlPowerupOption
    dw ControlInvincibilityOption
    dw ControlPlayerOption
    dw ControlCoinsOption
    dw ControlWarpsOption

;----------------------------------------------------------------

;"WARP TO WORLD  0-0    "
LevelOptionText:
    dw $2c20, $2c0a, $2c1b, $2c19, $2c28, $2c1d, $2c18, $2c28
    dw $2020, $2018, $201b, $2015, $200d, $2028, $2028, $2000
    dw $2024, $2000, $2c28, $2c28, $2c28, $2c28, $2c28, $2c28
    dw $2c28, $2c28

DrawLevelOption:
    lda !MenuHardModeFlag   ;hard mode enabled?
    beq +
    lda #$2a                ;write star to buffer if so
    sta !VRAM_BufferData+28
+:  lda !MenuWorldNumber    ;write world number
    inc                     ;increment for display, as usual
    sta !VRAM_BufferData+30
    lda !MenuLevelNumber    ;write level number
    inc                     ;increment for display, as usual
    sta !VRAM_BufferData+34
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
    beq +
    lda #$00                    ;disable hidden 1UP if on x-2 through x-4
    bra SetHidden1Up
+:  lda #$01                    ;enable hidden 1UP if on x-1 level
SetHidden1Up:
    sta !Hidden1UpFlag          ;to-do: give user control of hidden 1UP
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
    ;lda !LevelNumber            ;a bit hacky, but restart at pipe intro cutscene if we're on a x-2 level...
    ;cmp #!Level2                ;unfortunately, this is currently required because our RNG system assumes
    ;bne +                       ;a lives screen will occur before you spawn in the current/selected level
    ;sta !AreaNumber             ;we could probably fix this in the future by skipping RNG for lives screen
+:  stz !OperMode_Task          ;reset modes of operation
    lda #!GameModeValue
    sta !OperMode
    sta !FetchNewGameTimerFlag  ;set flag to reset game timer
    sta !ScreenFadeoutFlag      ;set flag to do fadeout animation
    sta !FixFadeoutBGScroll     ;set flag to maintain scroll value of BG on fadeout (also needed to clear practice menu)
    stz !MoveSpritesOffscreen   ;clear flag responsible for moving sprites offscreen
    stz !AltEntranceControl     ;reset entrance type
    stz !HalfwayPage            ;reset starting page to zero
    stz !GameTimerExpiredFlag   ;clear game timer expired flag to disable time-up screen
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
    ldx !CurrentGame            ;use our current game to load the correct area pointers
    beq +
    jsl !LoadAreaPointer_TLL
    bra ++
+:  jsl !LoadAreaPointer_SMB1
++: %menu_confirm_sfx()         ;play confirmation sound
    jmp ForceMenuClose          ;force the menu to close to start the game loop

ControlLevelOption:
    ldx !CurrentGame        ;store smb1/tll flag in X register
    lda !JoypadBits1Pressed ;check buttons just pressed
    bpl CheckForLevelChange ;if B button not pressed, check for directional input
    jmp WarpToLevel         ;otherwise warp to the currently-selected level
CheckForLevelChange:
    pha                     ;push to stack for later
    lda !JoypadBits1Held    ;check buttons currently held
    asl
    bmi ModifyWorldNum      ;if holding Y, modify world number
    pla                     ;check directions just pressed
    bit #%00000100
    bne DecrementLevel      ;if pressing down, decrement level
    bit #%00001000
    bne IncrementLevel      ;if pressing up, increment level
    rts                     ;if pressing neither, leave
ModifyWorldNum:
    pla                     ;check directions just pressed
    bit #%00000100
    bne DecWorldNum         ;if pressing down, decrement world
    bit #%00001000
    bne IncWorldNum         ;if pressing up, increment world
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
    lda !MenuEntranceFrame      ;take menu entrance fame and subtract 5 for display
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

HandleRNGNumber:
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
    lda !JoypadBits1Pressed ;check buttons just pressed
    bpl +                   ;if we haven't pressed B to confirm, branch
    %menu_confirm_sfx()     ;otherwise play confirm sfx
    inc !DrawOptionFlag     ;set flag to redraw option
    jmp SetRNGFromNumber    ;and save our new RNG/entrance values
+:  bit #%00000001
    bne IncEntranceSel      ;if pressing right, increment entrance selection
    bit #%00000010
    bne DecEntranceSel      ;if pressing left, decrement entrance selection
    ldx !MenuSelectionIndex ;get menu selection index
    beq HandleRNGNumber     ;do RNG number high byte if zero
    dex
    beq HandleRNGNumber     ;do RNG number low byte if one
    dex
    bne HandleScreenFlag    ;do screen flag if three
    bit #%00000100          ;otherwise we're doing the entrance frame (i.e. framerule timer)
    bne DecrementEntrance   ;if pressing down, increment entrance frame
    bit #%00001000
    bne IncrementEntrance   ;if pressing up, increment entrance frame
    rts
IncEntranceSel:
    %increment_option(!MenuSelectionIndex,#4)
DecEntranceSel:
    %decrement_option(!MenuSelectionIndex,#4)
DecrementEntrance:
    %decrement_option(!MenuEntranceFrame,#21)
IncrementEntrance:
    %increment_option(!MenuEntranceFrame,#21)
RedrawEntranceDisplay:
    %edit_value_sfx()
    inc !DrawOptionFlag
    rts
HandleScreenFlag:
    bit #%00000100
    bne InvertScreenFlag   ;if pressing down, invert screen flag
    bit #%00001000
    bne InvertScreenFlag   ;if pressing up, invert screen flag
    rts
InvertScreenFlag:
    %invert_option(!MenuScreenFlag)

;----------------------------------------------------------------

;"RNG = 00 00 00 00 00 00 00"
RNGOptionText:
    dw $2c1b, $2c17, $2c10, $2c28, $2c5e, $2c28, $2c00, $2c00
    dw $2c28, $2c00, $2c00, $2c28, $2c00, $2c00, $2c28, $2c00
    dw $2c00, $2c28, $2c00, $2c00, $2c28, $2c00, $2c00, $2c28
    dw $2c00, $2c00

;pretty bad because this assumes high byte never changes
DrawRNGOption:
    ldx #$06                    ;start with last byte of RNG
-:  ldy !SavedRNGBytes,x        ;load saved RNG byte in Y
    phx                         ;push index to stack for now
    txa                         ;multiply index by 6 for correct offset
    asl
    sta $00
    asl
    clc
    adc $00
    tax
    tya                         ;transfer RNG byte to A for routine
    jsr GetNybbles
    sta !VRAM_BufferData+14,x   ;write byte to buffer
    tya
    sta !VRAM_BufferData+12,x
    txy                         ;store index * 6 in Y
    plx                         ;restore original X
    cpx !MenuSelectionIndex     ;are we on the selected byte?
    bne +                       ;no, branch to decrement index
    lda #$20                    ;yes, color selected byte
    sta !VRAM_BufferData+13,y
    sta !VRAM_BufferData+15,y
+:  dex
    bpl -                       ;repeat until all seven bytes drawn
    rts

ControlRNGOption:
    ldx !MenuSelectionIndex ;load index into X register
    lda !JoypadBits1Held    ;check buttons held
    asl
    bmi RNGHighNybble       ;if Y button held, do high nybble
    lda !JoypadBits1Pressed ;otherwise check directions just pressed
    bit #%00000001
    bne DoNextRNGByte       ;if pressing right, go to next byte
    bit #%00000010
    bne DoPrevRNGByte       ;if pressing left, go to previous byte
    bit #%00000100
    bne DecrementRNG        ;if pressing down, decrement RNG byte
    bit #%00001000
    bne IncrementRNG        ;if pressing up, increment RNG byte
    rts
DoNextRNGByte:
    %increment_option(!MenuSelectionIndex,#7)
DoPrevRNGByte:
    %decrement_option(!MenuSelectionIndex,#7)
DecrementRNG:
    dec !SavedRNGBytes,x    ;decrement selected byte
    bra RedrawRNG
IncrementRNG:
    inc !SavedRNGBytes,x    ;increment selected byte
RedrawRNG:
    %edit_value_sfx()
    inc !DrawOptionFlag     ;redraw option
    rts
RNGHighNybble:
    lda !JoypadBits1Pressed ;check directions just pressed
    bit #%00000100
    bne DecRNGHigh          ;if pressing down, decrement RNG high nybble
    bit #%00001000
    bne IncRNGHigh          ;if pressing up, increment RNG high nybble
    rts
DecRNGHigh:
    lda !SavedRNGBytes,x    ;subtract $10 from byte
    sec
    sbc #$10
    bra StoreRNGByte
IncRNGHigh:
    lda !SavedRNGBytes,x    ;add $10 to byte
    clc
    adc #$10
StoreRNGByte:
    sta !SavedRNGBytes,x
    bra RedrawRNG

;----------------------------------------------------------------

;"RAM ADDRESSES = 0000 0000 "
RAMOptionText:
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
    lda !JoypadBits1Pressed ;now check directions just pressed
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
    lda !JoypadBits1Pressed ;check buttons just pressed
    bmi UpdatePowerup       ;if B button pressed, update our powerup state
    bit #%00000100
    bne DecrementPowerup    ;if pressing down, decrement powerup state
    bit #%00001000
    bne IncrementPowerup    ;if pressing up, increment powerup state
    rts                     ;if pressing neither, leave
UpdatePowerup:
    ldy #$00                ;set player size as big by default
    lda !MenuPlayerStatus   ;store menu powerup as actual powerup
    sta !PlayerStatus
    sta !SavedPlayerStatus  ;also update saved copy for level reload
    bne +                   ;if not small, branch
    iny                     ;otherwise increment for correct size
+:  sty !PlayerSize         ;set player size
    %powerup_sfx()
    rts                     ;leave
IncrementPowerup:
    %increment_option(!MenuPlayerStatus,#3)
DecrementPowerup:
    %decrement_option(!MenuPlayerStatus,#3)

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
    lda !JoypadBits1Pressed ;check buttons just pressed
    bpl +                   ;if B button not pressed, leave
    lda #$ff                ;otherwise give invincibility
    sta !StarInvincibleTimer
    %powerup_sfx()          ;play power-up sound
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
    lda !JoypadBits1Pressed ;check buttons just pressed
    bmi UpdatePlayer        ;if B button pressed, update current player
    bit #%00000100
    bne InvertPlayer        ;if pressing down, invert player
    bit #%00001000
    bne InvertPlayer        ;if pressing up, invert player
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
    lda !JoypadBits1Pressed ;check buttons just pressed
    bmi UpdateCoins         ;if B button pressed, update coin count
    pha                     ;retrieve pressed buttons later
    lda !JoypadBits1Held    ;check held buttons
    asl
    bmi CoinTens            ;if holding Y, do tens place
    pla
    bit #%00000100
    bne DecrementCoins      ;if pressing down, decrement coins
    bit #%00001000
    bne IncrementCoins      ;if pressing up, increment coins
    rts
UpdateCoins:
    lda !MenuCoinTally     ;update internal coin tally
    sta !CoinTally
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
    bit #%00000100
    bne SubTenCoins      ;if pressing down, decrement tens place
    bit #%00001000
    bne AddTenCoins      ;if pressing up, increment tens place
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
    lda !JoypadBits1Pressed ;check buttons just pressed
    bmi UpdateWarpsFlag     ;if B button pressed, update warps flag
    bit #%00000100
    bne InvertWarpsFlag     ;if pressing down, invert flag
    bit #%00001000
    bne InvertWarpsFlag     ;if pressing up, invert flag
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
    lda !JoypadBits1Held
    bit #%01000000          ;holding Y button?
    beq CheckAddSubValue    ;no, branch
    inx                     ;if holding Y, increment index for high nybble
CheckAddSubValue:
    lda !JoypadBits1Pressed ;now check directions just pressed
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