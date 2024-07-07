;smb1/smbtll addresses
;normal defines
!FrameCounter = $09
!GameEngineSubroutine = $0f
!Player_X_Speed = $5d
!DigitModifier = $0145
;!NMIFlag = $0154
!Player_X_Position = $0219
!Enemy_X_Position = $021a
!Player_Y_Position = $0237
!SprObject_X_MoveForce = $0401
!WarpZoneControl = $06d6
!Player_X_Scroll = $06ff
!ScreenEdge_X_Pos = $071c
!MoveSpritesOffscreen = $0722
!TimerControl = $0747
!EntrancePage = $0751
!AltEntranceControl = $0752
!SelectedPlayer = $0753
!PlayerSize = $0754
!PlayerStatus = $0756
!FetchNewGameTimerFlag = $0757
!GameTimerExpiredFlag = $0759
!NumberOfLives = $075a
!HalfwayPage = $075b
!LevelNumber = $075c
!Hidden1UpFlag = $075d
!CoinTally = $075e
!WorldNumber = $075f
!AreaNumber = $0760
!OffScr_NumberofLives = $0761
!OffScr_Hidden1UpFlag = $0764
!DisableIntermediate = $0769
!PrimaryHardMode = $076a
!OperMode = $0770
!OperMode_Task = $0772
!PauseModeFlag = $0776
!IntervalTimerControl = $0787
!JumpSwimTimer = $078a
!GameTimerCtrlTimer = $078f
!EnemyIntervalTimer = $07a2
!StarInvincibleTimer = $07af
!WorldEndTimer = $07b1
!PseudoRandomBitReg = $07b7
!CoinDisplay = $07de
!CompletedWorlds = $07fa ; seems to be a leftover from 2j fds
!HardWorldsFlag = $07fb ;tll letter worlds
!MoreDifficultQuestFlag = $07fc
!ScreenFadeoutFlag = $0e67
!FixFadeoutBGScroll = $0e7f
!CurrentBrother = $0ec2 ;yeah these are different for whatever reason
!WarpsUsedFlag = $0f2b ;disallows tll world 9 if non-zero
!JoypadBits1Held = $0ff4
!JoypadBits1Pressed = $0ff6
!JoypadBits2Held = $0ff8
!JoypadBits2Pressed = $0ffa
!VRAM_BufferOffset = $1700
!VRAM_BufferAddr = $1702
!VRAM_BufferLen = $1704
!VRAM_BufferData = $1706

;smb1/smbtll labels
!TitleScreenModeValue = 0
!GameModeValue = 1
!VictoryModeValue = 2
!GameOverModeValue = 3
!World1 = 0
!World2 = 1
!World3 = 2
!World4 = 3
!World5 = 4
!World6 = 5
!World7 = 6
!World8 = 7
!World9 = 8
!WorldA = 9
!WorldB = 10
!WorldC = 11
!WorldD = 12
!Level1 = 0
!Level2 = 1
!Level3 = 2
!Level4 = 3

!SkipRNGAndSound_SMB1 = $0381da
!WaitForNMI_SMB1 = $0382cf
!SoundEngine = $048163
!LoadAreaPointer_SMB1 = $04c00b
!ReadJoypads_SMB1 = $05c800
!SkipRNGAndSound_TLL = $0d8098
!WaitForNMI_TLL = $0d8181
!LoadAreaPointer_TLL = $0ec54c
!ReadJoypads_TLL = $0fd000

;custom defines
!MAX_OPTIONS_SMB1 = 7
!MAX_OPTIONS_TLL = 10

!WarpWorldNumber = $04fe
!LagCounter = $04ff

!PracticeMenuMemory = $0a20
!AllocatedMemory = 0
macro malloc_prac(id, size) ;thanks lui
	!<id> := !PracticeMenuMemory+!AllocatedMemory
	!AllocatedMemory #= !AllocatedMemory+<size>
endmacro

%malloc_prac(PracticeMenuFlag,1)
%malloc_prac(PracticeMenuOption,1)
%malloc_prac(DrawOptionFlag,1)
%malloc_prac(MenuCloseDelay,1)
%malloc_prac(CurrentGame,1) ;0 for smb1, 1 for tll
%malloc_prac(MenuHardModeFlag,1)
%malloc_prac(MenuWorldNumber,1)
%malloc_prac(MenuLevelNumber,1)
%malloc_prac(MenuPlayerStatus,1)
%malloc_prac(CustomAddressA,2)
%malloc_prac(CustomAddressB,2)
%malloc_prac(MenuSelectedPlayer,1)
%malloc_prac(MenuCoinTally,1)
%malloc_prac(MenuSelectionIndex,1)
%malloc_prac(SavedRNGBytes,7)
%malloc_prac(SavedEntranceFrame,1)
%malloc_prac(MenuEntranceFrame,1)
%malloc_prac(SavedScreenFlag,1)
%malloc_prac(MenuScreenFlag,1)
%malloc_prac(DelayRNGFlag,1)
%malloc_prac(MenuWarpsFlag,1)
%malloc_prac(SavedPlayerStatus,1)
%malloc_prac(CurrentRNGNumber,2)
%malloc_prac(SavedRNGNumber,2)
%malloc_prac(MenuRNGNumber,2)
%malloc_prac(SavedIntermediateFlag,1)
%malloc_prac(Saved1UpFlag,1)
%malloc_prac(Menu1UpFlag,1)
%malloc_prac(SavedEntrancePage,1)

;macros
macro setup_vram_buffer(addr,len)
    rep #$30                 ;enable 16-bit registers
    ldx !VRAM_BufferOffset
    lda #<addr>              ;location of text, LSB then MSB
    sta !VRAM_BufferAddr,x
    lda #<len>               ;length of text, LSB then MSB
    sta !VRAM_BufferLen,x
endmacro

macro update_buffer_offset(len)
    rep #$30                 ;enable 16-bit registers
    lda #$ffff               ;apply terminator to end of VRAM buffer
    sta !VRAM_BufferData+<len>+1,x
    txa
    clc
    adc #<len>+5
    sta !VRAM_BufferOffset   ;update VRAM buffer offset accordingly
    sep #$30                 ;disable 16-bit registers
endmacro

macro increment_option(addr,len)
    lda <len>   ;store max selections here
    sta $02
    rep #$20
    lda #<addr> ;address of option
    sta $00
    sep #$20
    jmp IncrementOption
endmacro

macro decrement_option(addr,len)
    lda <len>   ;store max selections here
    sta $02
    rep #$20
    lda #<addr> ;address of option
    sta $00
    sep #$20
    jmp DecrementOption
endmacro

macro invert_option(addr)
    rep #$20
    lda #<addr> ;address of option
    sta $00
    sep #$20
    jmp InvertOption
endmacro

macro powerup_sfx()
	lda #$58
	sta $1603
endmacro

macro coin_sfx()
	lda #$01
	sta $1603
endmacro

macro pipe_sfx()
	lda #$04
	sta $1600
endmacro

macro menu_open_sfx()
    lda #$0e
    sta $1603
endmacro

macro change_option_sfx()
    lda #$23
    sta $1603
endmacro

macro edit_value_sfx()
	lda #$4c
	sta $1603
endmacro

macro menu_confirm_sfx()
	lda #$29
	sta $1603
endmacro

macro menu_close_sfx()
	lda #$20
	sta $1600
endmacro