@echo off
if not exist "sfc\smc.sfc" (
	echo "smc.sfc" is not in the sfc folder.
	pause
	exit
)


if not exist "sfc\smc_prac.sfc" (
	copy sfc\smc.sfc sfc\smc_prac.sfc
)

if not exist "sfc\smc_prac_ss.sfc" (
	copy sfc\smc.sfc sfc\smc_prac_ss.sfc
)

asar.exe --fix-checksum=on --define savestates=0 src/main.asm sfc/smc_prac.sfc
asar.exe --fix-checksum=on --define savestates=1 src/main.asm sfc/smc_prac_ss.sfc
pause