;move timer to the left by one tile
org $039c93
    db $79

;print frames remaining in game timer
org $03b7f3
    jsr UpdateTimer_SMB1
org $039d66
UpdateTimer_SMB1:
    jsl PrintTimerFrame
    rts

;print remainder at end of flagpole stage
org $03d878
    jsl PrintRemainderFlagpole
    nop

;print remainders for castle stages
org $038ac7
    jsl PrintRemainderCastle_6
    nop
org $0389c2
    jsl PrintRemainderCastle_6
    nop

;update frame counter for sideways pipe entry
org $03e4bd
    jsl SidewaysPipeEntry

;update frame counter for vertical pipe entry
org $03e647
    jsl VerticalPipeEntry

;alter status bar nybbles to not print score (or coins for smb1 specifically)
org $03bd2a
    db $aa,$aa

;update frame counter on timer countdown instead of score
org $03d81f
    jsl UpdateFrameCounter
    lda #$0a

;skip score zero supression to prevent vram overwrite
org $03bd6d
    bra $0e
org $049366 ;is this necessary??
    bra $05

;sockfolder position hijack (does backwards flag math too)
org $03ae4e
    jsl BackwardsFlagValue

;don't subtract lives
org $03a09b
    stz !NumberOfLives

;init lives to 1 when starting game
org $05c980
    stz !NumberOfLives
org $05c987
    stz !OffScr_NumberofLives

;print frame counter when jumping/swimming
org $03b50a
    jsl PlayerJumpTimer
    nop

;print frame counter instead of updating score
org $03bd58
    jsl UpdateFrameCounter
    nop
    nop

;clear out digit modifiers for points (prevents erroneous additions to timer)
org $038b6f
    stz !DigitModifier,x
org $03b948
    stz !DigitModifier,x
org $03bd50 ;also skip frame counter update on coin collection
    stz !DigitModifier+4
    bra $09
;these ones don't seem necessary, but good precedent
org $03bfbc ;also skip frame counter update on brick break
    stz !DigitModifier+5
    nop
    nop
    nop
org $03d7a7
    stz !DigitModifier+4
org $03d813
    stz !DigitModifier+5

;display frame counter on axe grab
org $03e4e7
    jsl PlayerAxeGrab

;practice menu hijack (we do it here to pause RNG too)
org $0381a9
    jsl PracticeMenu_SMB1

;TopStatusBarLine replacement
org $038f23
    db $58,$4b,$00,$01,$0f,$2c ; "F" for frame counter
    db $58,$54,$80,$03,$0a,$2c ; "A" for RAM address A
    db $0b,$2c ; "B" for RAM address B
    db $58,$59,$00,$07,$1D,$20,$12,$20,$16,$20,$0E,$20 ; "TIME"
    db $58,$6b,$00,$07,$5f,$2c,$25,$20,$00,$20,$00,$20 ; "|>x00" for backwards flagpole
    db $ff,$ff ; unused bytes

;BottomStatusLine replacement (hacky)
org $038c8d
    %setup_vram_buffer($4958,$0380)
    lda #$2c25                  ;"x" for entrance frame indicator
    sta !VRAM_BufferData,x
    sep #$30
    lda #$20                    ;palette and priority for entrance frame
    sta !VRAM_BufferData+3,x
    lda !IntervalTimerControl   ;print framerule value for entrance frame
    sta !VRAM_BufferData+2,x
    phx
    ldy #$00
-:  lda SockfolderText_SMB1,y   ;print remaining bottom status bar text
    sta !VRAM_BufferData+4,x
    inx
    iny
    cpy #22
    bcc -
    lda #$ff                    ;append terminator
    sta !VRAM_BufferData+4,x
    plx
    txa
    clc
    adc #30                     ;move buffer offset up
    sta !VRAM_BufferOffset
    jmp $8f08                   ;jump to handle next screen task
SockfolderText_SMB1:
    dw $6358,$0100,$2c1c ;"S"
    dw $6458,$0740,$2000 ;"0000" after S
    dw $7058,$0500,$2c15,$2000,$2000 ;"L00" for lag counter

warnpc $038cfe

;save RNG and entrance frame when loading area pointer (need to do warpzones)
org $0387eb ;title screen (reorder code to store hidden 1-UP flag first)
    inc !Hidden1UpFlag
    inc !OffScr_Hidden1UpFlag
    jsl LoadAreaPointerHijack_SMB1
org $038afa ;world end
    jsl LoadAreaPointerHijack_SMB1
org $03a234 ;death, game over, princess rescue
    jsl LoadAreaPointerHijack_SMB1
org $03b368 ;level end (we reorder these subs so $0e7f is set before hijack)
    jsr $b248
    inc !FetchNewGameTimerFlag
    jsl LoadAreaPointerHijack_SMB1
org $03b250 ;warpzone end
    jsl ChgAreaModeHijack
    nop

;level timer & lag counter hack
org $038316
    jsr DoTimerLag_SMB1
org $039d6b
DoTimerLag_SMB1:
    jsl UpdateLevelTimer
    rts

;update RNG number
org $0381d6
    jsl UpdateRNGNumber

;print frame counter when player gains control
org $03afd2
    jsl PlayerRdy

;store world number into seperate address when taking warp zone (bugfix for quick restart)
org $03e689
    sty !WarpWorldNumber
org $03e6a3 ;don't reset level/area numbers when taking warp zone
    bra $04
    nop
    nop
    nop
    nop

;hijack to set custom addresses on game boot
org $03817e
    jsl InitCustomAddresses

;do not draw world/level numbers in 2 player game
org $049366
    bra $5f

;display frame counter on bowser spawn
org $03c93f
    jsl BowserSpawn

;hijack to render level timer on lives screen
org $038d96
    jsl RenderLevelTimer_SMB1
