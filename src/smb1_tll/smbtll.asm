;move timer to the left by one tile
org $0d9819
    db $79

;print frames remaining in game timer
org $0db580
    jsr UpdateTimer_TLL
org $0d98d8
UpdateTimer_TLL:
    jsl PrintTimerFrame
    rts

;print remainder at end of flagpole stage
org $0dd74a
    jsl PrintRemainderFlagpole
    nop

;print remainders for castle stages
org $0d884f
    jsl PrintRemainderCastle_8
    nop
org $0d8768
    jsl PrintRemainderCastle_6
    nop

;update frame counter for sideways pipe entry
org $0de3ed
    jsl SidewaysPipeEntry

;update frame counter for vertical pipe entry
org $0de58a
    jsl VerticalPipeEntry

;alter status bar nybbles to not print score
org $0dbadc
    db $a2,$a3

;update frame counter on timer countdown instead of score
org $0dd6f1
    jsl UpdateFrameCounter
    lda #$0a

;skip score zero supression to prevent vram overwrite
org $0dbb18
    bra $0e

;sockfolder position hijack
org $0dac56
    jsl UpdateSockfolderPos

;don't subtract lives
org $0d9f59
    stz !NumberOfLives

;init lives to 1 when starting game
org $0fd0b0
    stz !NumberOfLives

;print frame counter when jumping/swimming
org $0db271
    jsl PlayerJumpTimer
    nop

;print frame counter instead of updating score
org $0dbb06
    jsl UpdateFrameCounter
    nop
    nop

;clear out digit modifiers for points (prevents erroneous additions to timer)
org $0d8916
    stz !DigitModifier,x
org $0db6e1
    stz !DigitModifier,x
org $0dbb03
    stz !DigitModifier+4
;these ones don't seem necessary, but good precedent
org $0dbd89
    stz !DigitModifier+5
org $0dd67b
    stz !DigitModifier+4
org $0dd6ec
    stz !DigitModifier+5

;display frame counter on axe grab
org $0de415
    jsl PlayerAxeGrab

;practice menu hijack (we do it here to pause RNG too)
org $0d8067
    jsl PracticeMenu_TLL

;TopStatusBarLine replacement
org $0d8be3
    db $58,$4b,$00,$01,$0f,$2c ; "F" for frame counter
    db $58,$54,$80,$03,$0a,$2c ; "A" for RAM address A
    db $0b,$2c ; "B" for RAM address B
    db $58,$59,$00,$07,$1D,$20,$12,$20,$16,$20,$0E,$20 ; "TIME"
    db $58,$6b,$00,$03,$27,$24,$25,$20 ; "cx" for coin count
    db $ff,$ff,$ff,$ff,$ff,$ff ; unused bytes
org $0d8c1a ;seems like an extra copy that doesn't need updating, but hey why not
    db $58,$4b,$00,$01,$0f,$2c ; "F" for frame counter
    db $58,$54,$80,$03,$0a,$2c ; "A" for RAM address A
    db $0b,$2c ; "B" for RAM address B
    db $58,$59,$00,$07,$1D,$20,$12,$20,$16,$20,$0E,$20 ; "TIME"
    db $58,$6b,$00,$03,$27,$24,$25,$20 ; "cx" for coin count
    db $ff,$ff,$ff,$ff,$ff,$ff ; unused bytes

;BottomStatusLine replacement (hacky)
org $0d8a5a
    %setup_vram_buffer($4958,$0380)
    lda #$2c0e
    sta !VRAM_BufferData,x
    sep #$30
    lda #$20
    sta !VRAM_BufferData+3,x
    lda !IntervalTimerControl
    sta !VRAM_BufferData+2,x
    phx
    ldy #$00
-:  lda SockfolderText_TLL,y
    sta !VRAM_BufferData+4,x
    inx
    iny
    cpy #22
    bcc -
    lda #$ff
    sta !VRAM_BufferData+4,x
    plx
    txa
    clc
    adc #30
    sta !VRAM_BufferOffset
    jmp $8bc8
SockfolderText_TLL:
    dw $6358,$0100,$2c1c ;"S"
    dw $6458,$0740,$2000 ;"0000" after S
    dw $7058,$0500,$2c15,$2000,$2000 ;"L00" for lag counter

;save RNG and entrance frame when loading area pointer
org $0d861d ;title screen (reorder code to store hidden 1-UP flag first)
    inc !Hidden1UpFlag
    inc !OffScr_Hidden1UpFlag
    jsl LoadAreaPointerHijack_TLL
org $0d8890 ;world end
    jsl LoadAreaPointerHijack_TLL
org $0d9fd9 ;death, game over, princess rescue
    jsl LoadAreaPointerHijack_TLL
org $0db0b5 ;level end (we reorder these subs so $0e7f is set before hijack)
    jsr $af6d
    inc !FetchNewGameTimerFlag
    jsl LoadAreaPointerHijack_TLL
    stz !HalfwayPage ;fix for tll, reordering routines no longer guarantees $00 in A
org $0daf82 ;warpzone end
    jsl ChgAreaModeHijack
    nop

;lag counter hack
org $0d81a7
    jmp DoLag_TLL
org $0d98dd
DoLag_TLL:
    jsl UpdateLagCounter
    jmp $82ec

;update RNG number
org $0d8094
    jsl UpdateRNGNumber

;print frame counter when player gains control
org $0dadde
    jsl PlayerRdy

;store world number into seperate address when taking warp zone (bugfix for quick restart)
org $0de5b4
    sty !WarpWorldNumber
org $0de5d1 ;don't reset level/area numbers when taking warp zone
    nop
    nop
    nop
    nop
    nop
    nop