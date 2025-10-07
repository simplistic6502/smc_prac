lorom

;include defines
incsrc "defines.asm"

;smb1 and smbtll
incsrc "smb1_tll/common.asm"
incsrc "smb1_tll/smb1.asm"
incsrc "smb1_tll/smbtll.asm"
incsrc "smb3/smb3.asm"

if !savestates
	incsrc "savestates.asm"
endif

org $02bc00
incbin "customtitle.bin"