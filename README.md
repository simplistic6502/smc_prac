This is a ROM hack for Super Mario Collection that adds tools for easier speedrun practice. Currently, only Super Mario Bros. & The Lost Levels have been modified, but Super Mario Bros. 2 and Super Mario Bros. 3 will recieve practice modifications in the future. Apply the IPS patch onto a Super Mario Collection J1.0 ROM (an example MD5 hash is `0b6d70d07523a8804cbbb94b154510a2`).

# Super Mario Bros. & The Lost Levels

## Features
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
	- Set RNG number, entrance frame, and fade-out flag to practice specific patterns
	- View and freely modify the seven RNG bytes
	- Set the two user-defined addresses to desired LowRAM values
	- Set power-up state, with selections also applying to level select and quick restart
	- Enable or disable the hidden 1-Up Mushrooms in the first level of each world
	- Grant invincibility to practice sections without risk of contact damage
	- Switch between Mario and Luigi (for The Lost Levels)
	- Set current coin count to practice firework avoidance (for The Lost Levels)
	- Set flag to enable/disable World 9 upon 8-4 completion (for The Lost Levels)
- Quick restart feature using L+R shoulder buttons to reload the current level with RNG intact
- Infinite lives, so you never have to worry about getting a game over

## How to Use
- Press select to open/close to open the practice menu, can only be opened while in normal gameplay.
- Press L shoulder button to go back an option, press R shoulder button to go forward an option.
- Pressing up or down increments or decrements the selected option.
- Hold Y while pressing up or down to modify the higher digit of the selected option instead.
- Press B to confirm the selected option.
- Hold L and R shoulder buttons while in normal gameplay to force the current level to reload.
- To set the correct patterns when using the warp or level reload features, use the entrance option. Set the RNG number from 0001 to 7FFF (or 0000 to use the current RNG), the entrance frame, and the fade-out flag, then press B to confirm.
- If the previous level was ended via flagpole, entrance frame is E. If the previous level was ended via the standard world clear sequence, entrance frame is F. Otherwise, entrance frame is variable.
- The fade-out flag is 0 if the current level was entered via the title screen or the standard world clear sequence, otherwise it is 1.
- Any changes made to the seven RNG bytes in the practice menu will immediately take effect upon using the warp or level reload features. If you don't want to set the RNG bytes manually, use the RNG number in the entrance menu.

## Known Issues/Quirks
- Any changes made to the seven RNG bytes in the practice menu will immediately take effect upon using the warp or level reload features. If you don't want to set the RNG bytes manually, use the RNG number under the entrance option instead
- If the current level is reload or a new level is selected while the music is sped up from low time remaining, music will continue to be fast until a theme that cannot be sped up plays.

# Changelog
- v1.0.1
	- Fixed an issue where entering the exit pipe in 1-2 or 4-2 of Super Mario Bros. would erroneously update the saved game state.
	- Fixed an issue where using the level reload feature while entering a warp zone pipe would take you to the new level instead.
	- Fixed an issue where the max falling speed of the rescued Toads during the 6-4 and B-4 end-of-level sequences was overwritten, slowing the animation down.
	- Added an option to enable or disable the hidden 1-Up Mushroom blocks and added the ability to save this option when reloading a level or warping to a new level.
	- The entrance page value at the start of the level is now restored when using the level reload feature.

- v1.0.0
	- Initial release, added practice support for Super Mario Bros. & The Lost Levels.

# Credits
- Lui37, for his work on a Super Mario All-Stars practice ROM for The Lost Levels that was used as a reference for this project.
- pellsson, for his work on practice utilities for NES Super Mario Bros./FDS The Lost Levels that served as a basis for this project's feature implementations.
- threecreepio, for his notes and implementation of the Sockfolder position display.