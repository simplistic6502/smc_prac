@echo off
asar.exe --fix-checksum=on --define savestates=0 src/main.asm sfc/smc_prac.sfc
asar.exe --fix-checksum=on --define savestates=1 src/main.asm sfc/smc_prac_ss.sfc
pause