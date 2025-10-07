This is a ROM hack for Super Mario Collection that adds tools for easier speedrun practice. Currently, only Super Mario Bros. & The Lost Levels have recieved specific practice modifications, but Super Mario Bros. 2 and Super Mario Bros. 3 will be supported in the future. `smc_prac_ss.ips` offers savestate support, but not every platform permits ROMs with 256KB SRAM (such as the Super EverDrive X5/X6). `smc_prac.ips` lacks savestates but is supported by every platform that can run the original game. Apply either IPS patch onto a Super Mario Collection J1.0 ROM (an example MD5 hash is `0b6d70d07523a8804cbbb94b154510a2`).

# Features
## All Games
- The ability to make a battery-backed savestate that can be be loaded at any time in the game the savestate was created.

## Super Mario Bros. & The Lost Levels:
- Custom status bar with additional information
	- Display for frames remaining in the current in-game timer tick
	- Frame counter display for key actions
	- Framerule remainder display at end of levels
	- Sockfolder-style position display
	- Sockfolder-style entrance frame/room score display
	- Two user-defined addresses in SNES LowRAM ($0000-$1FFF)
	- Lag counter display, cleared on room entry
	- Result of computation that causes backwards flagpole grab (for SMB1)
- Practice menu with options, opened using the select button
	- Warp to any level, including levels with hard mode enabled
	- Set power-up state, with selections also applying to level select and quick restart
	- Set RNG number, entrance frame, and fade-out flag to practice specific patterns
	- View and freely modify the seven RNG bytes
	- Set current coin count to practice firework avoidance (for The Lost Levels)
	- Set the two user-defined addresses to desired LowRAM values
	- Switch between Mario and Luigi (for The Lost Levels)
	- Set flag to enable/disable World 9 upon 8-4 completion (for The Lost Levels)
	- Enable or disable the hidden 1-Up Mushrooms in the first level of each world
	- Grant invincibility to practice sections without risk of contact damage
	- Return to the title screen at any time, resetting RNG in the process
- Quick restart feature using L+R shoulder buttons to reload the current level with RNG intact
- Infinite lives, so you never have to worry about getting a game over

# How to Use
- A savestate is created by holding the R shoulder button and pressing select. The savestate can then be loaded by holding the L shoulder button and pressing select. Savestates may only be loaded in the same game they were originally made.

## Super Mario Bros. & The Lost Levels
- Press select to open/close to open the practice menu; note that the practice menu can only be opened while in normal gameplay.
- Press L shoulder button to go back an option, press R shoulder button to go forward an option.
- Pressing up or down increments or decrements the current selection. For options without multiple selections, right and left can be used to increment and decrement as well.
- For options with multiple selections, use left and right to navigate between selections. For four-digit hex values, the low and high bytes are considered seperate selections.
- Hold Y to modify the higher digit of the current selection instead.
- Press B to confirm the selected option and apply its effects.
- Hold L and R shoulder buttons while in normal gameplay to force the current level to reload.
- To set the correct patterns when using the warp or level reload features, use the entrance option. Set the RNG number from $0001 to $7FFF (or $0000 to use the current RNG), the entrance frame, and the fade-out flag, then press B to confirm.
- If the previous level was ended via flagpole, entrance frame is E. If the previous level was ended via the standard world clear sequence, entrance frame is F. Otherwise, entrance frame is variable.
- The fade-out flag is 0 if the current level was entered via the title screen or the standard world clear sequence, otherwise it is 1.
- Hold X while modifying the RNG number to increment or decrement by one framerule.
- Press A while under the entrance option to overwrite the saved RNG with the current RNG (same effect as RNG number $0000). Useful for quickly setting RNG to a psuedo-random value.
- Any changes made to the seven RNG bytes in the practice menu will immediately take effect upon using the warp or level reload features. If you don't want to set the RNG bytes manually, use the RNG number in the entrance menu.

# Known Issues/Quirks
- If the current level is reloaded or a new level is selected while the music is sped up from low time remaining, music will continue to be fast until a theme that cannot be sped up plays.
- Loading a savestate may result in incorrect music playing, most notably in Super Mario Bros. 3 due to that game switching audio banks during gameplay.

# Changelog
- v1.1.0
	- Added savestate support for platforms that permit at least 256KB of SRAM. A version of the practice hack without savestates is also offered for the sake of compatibility.
	- Coin count is now saved when entering a new level so that it can be restored when using level select or quick restart. Changes to the coin count in the practice menu overwrite the saved copy, much like power-up status.
	- The option to return to the title screen has been added to the practice menu, meaning that pressing reset is no longer required to reload the title screen or game selection menu.
	- The A button may now be pressed while under the entrance option to "scramble" RNG by overwriting the saved RNG with current RNG.
	- Holding the X button while modifying the RNG number will now cause the RNG number to go up/down by 21 to simulate framerules.
	- The custom LowRAM addresses are initialized to $03AD (horizontal position relative to the left side of the screen) and $0705 (horizontal subspeed), respectively, when starting Super Mario Bros. or The Lost Levels.
	- The practice menu has been reorganized for quicker access to frequently-used options.
	- Incrementing and decrementing with right and left is now allowed for options without more than one selection.
	- Visual improvements to the practice menu have been made, specifically cursor icons on the side of the menu and dynamic spacing for the level selection option.
	- Setting power-up state to small in the practice menu now plays the damage sound instead of the power-up sound.
	- Fixed an issue where terrain would fail to generate if warping to an underground or underwater stage or returning to the title screen from the 8-4/D-4 ending sequence.
	- Fixed an issue where warping to an underground or underwater stage from 8-4 would fail to play music.
	- Fixed a couple visual issues present when selecting 2 player game in Super Mario Bros.

- v1.0.1
	- Fixed an issue where entering the exit pipe in 1-2 or 4-2 of Super Mario Bros. would erroneously update the saved game state.
	- Fixed an issue where using the level reload feature while entering a warp zone pipe would take you to the new level instead.
	- Fixed an issue where the max falling speed of the rescued Toads during the 6-4 and B-4 end-of-level sequences was overwritten, slowing the animation down.
	- Added an option to enable or disable the hidden 1-Up Mushroom blocks and added the ability to save this option when reloading a level or warping to a new level.
	- The entrance page value at the start of the level is now restored when using the level reload feature.

- v1.0.0
	- Initial release, added practice support for Super Mario Bros. & The Lost Levels.

# Credits
- Lui37, for his work on a Super Mario All-Stars practice ROM for The Lost Levels that was used as a reference for this project, particularly the savestate code.
- pellsson, for his work on practice utilities for NES Super Mario Bros./FDS The Lost Levels that served as a basis for this project's feature implementations.
- threecreepio, for his notes and implementation of the Sockfolder position display.