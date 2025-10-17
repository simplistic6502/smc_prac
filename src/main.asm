lorom

;include defines
incsrc "defines.asm"

;smb1 and smbtll
incsrc "smb1_tll/common.asm"
incsrc "smb1_tll/smb1.asm"
incsrc "smb1_tll/smbtll.asm"

;smb3
incsrc "smb3/edits.asm"
incsrc "smb3/speedview.asm"
incsrc "smb3/subpixel.asm"
incsrc "smb3/mapedits.asm"

if !savestates
	incsrc "savestates.asm"
endif

org $02bc00
incbin "customtitle.bin"