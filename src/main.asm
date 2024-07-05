lorom

;include defines
incsrc "defines.asm"

;smb1 and smbtll
incsrc "smb1_tll/common.asm"
incsrc "smb1_tll/smb1.asm"
incsrc "smb1_tll/smbtll.asm"

org $02bc00
incbin "customtitle.bin"