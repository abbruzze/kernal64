[![Build Status](https://app.travis-ci.com/abbruzze/kernal64.svg?branch=master)](https://app.travis-ci.com/abbruzze/kernal64)
[![Release](https://img.shields.io/github/v/release/abbruzze/kernal64)](https://github.com/abbruzze/kernal64/releases)
[![Language](https://img.shields.io/github/languages/top/abbruzze/kernal64)]()
[![Downloads](https://img.shields.io/github/downloads/abbruzze/kernal64/total)](https://github.com/abbruzze/kernal64/releases/latest)

Kernal64 ver 1.7.4b1
========
![](https://github.com/abbruzze/kernal64/blob/master/images/c64.jpg)![](https://github.com/abbruzze/kernal64/blob/master/images/c128.jpg)
<img src="https://github.com/abbruzze/kernal64/blob/master/images/commodore128_vdc.jpg" alt="Commodore 128 VDC"/>
![](https://github.com/abbruzze/kernal64/blob/master/images/kernal64_scpu.gif)

### Installation
Go to https://github.com/abbruzze/kernal64/releases/latest and download and unzip on your computer the latest version.
Be sure to have a jre (11 or above) in the path and launch:
- On Windows: **k64.bat**, **kscpu64.bat** or **k128.bat**. If you want the shell support (for --help command line, for example) you could use the **k64s.bat**, **kscpu64s.bat** or **k128s.bat** scripts.
- On Linux: **k64.sh**, **kscpu64.sh** or **k128.sh**.

The scripts start java with the -server option for performance reason. It could happen that your Java installation does not support the server option. In this case edit the script and remove the -server option.

If you want to load a 16M REU modify the memory settings inside the start scripts: for example, you could set -Xmx128M.

### Wiki
Wiki pages are available here: https://github.com/abbruzze/kernal64/wiki

### What's new 1.7.4b1 (Jul 29th 2022)
- General
  - Added support for G71 disk format
  - Fixed state saving support: broken on last Wic64 release
  - Adjusted LAST_SECTOR_GAP in gcr handling to a larger value: demo E2IRA now runs fine (except for a part into disk B that is not visible; border must be turned off !!)
- C128
  - Added support for MagicDesk128 cart (https://github.com/RetroNynjah/Magic-Desk-128). It can be possible to run VolleyForTwo game with cartridge (https://kollektivet.nu/v42/)
- BeamRacer
  - Fixed 0x43 registry handling. Now silverdr's demo jema_n_silver works fine. Thanks Silverdr for your help in testing.

### What's new 1.7.3b16 (Apr 10th 2022)
- C64, C128, SCPU
  - Fixed hard reset for expansion port 
  - WiC64 emulation: new dialog from Settings -> I/O. 
    - Added fix: url encoding removed.
    - Added: state saving
    - Added --wic64-network-name --wic64-enabled command line options
    - Fixed MAC ADDRESS encoding
    - Added support for SCPU
    - Fixed response in case of bad URL
    - Improved debug messages
    - Added support for $24 command (POST): to be verified.
    - Fixed setMode (to fix NUFLI demo)
    - Background GET download postponed to fix corner cases
  - Fixed video cache (black line on last video line)
- C128
  - Fixed page 0 & 1 redirection for common memory.
- Beam Racer
  -  Fixed handling of bank pointers.  
- Joystick
  - Fixed keyboard emulation 
- REU
  - Added support for 8M 
- 6510
  - Commented out dma check before writing

### What's new in 1.7.3b4 (Feb 15th 2022) BETA
- C64 & C128
  - **WiC64 emulation**: new dialog from Settings -> I/O. Visit https://www.wic64.de/ for more information or https://www.forum64.de/. The emulation covers the firmware release 3.0 and some other undocumented feature like streaming. The firmware is still under development, so be patient...
  - Fixed video cache (black line on last video line)
- C128
  - Fixed page 0 & 1 redirection for common memory.
- Beam Racer
  -  Fixed handling of bank pointers.  

To download the BETA version go to https://github.com/abbruzze/kernal64/releases/tag/v1.7.3b4

### What's new in 1.7.2_b1 (Nov 29th 2021)
- General
  - CPU: Fixed wrapping for branch instructions
  - CPU: Fixed SLO: shift left could overflow
  - Fixed CSDB and Pouet spi: changed http to https references
- BeamRacer
  - Two memory pointers are now used: one for running program and one for ports

### What's new in 1.7.1_b14 (May 24th 2021)
- General
  - Rollback of VIC's pipeline function: unfortunately it's not yet full functional; more VICE's testbench tests passed but more demos broken.
  - Debugger: registers are now shown in a table
  - REU: changed floatingBus emulation, still something to fix.
  - Fixed --warp option: now disables audio output
  - Added --custom-glue-logic option
  - Fixed handling of undocumented opcodes when operate in decimal mode.
  - Fixed state saving: included PAL/NTSC VIC'state
  - Fixed handling of CPU port's fading bits (6,7 for C64, 7 for C128)
  - Added external -Dkernal64.config property to specify the configuration directory.
  - Added new --viciinew option.
  - Added 1024,2048,4096 sizes to --reu-type option.
  - Fixed handling of warp mode: now when switching back to normal speed ther's no freeze time to wait for.
  - CIA: fixed light-pen handling
  - new SID (resid) implementation
  - REU: when REU asserts DMA, AEC is high, and every pending writes are lost.
  - Datassette: fixed counter on recording
  - GMOD3 cartridge implementation
  - New KCS cart
- C128
  - VDC: fixed cursor handling introduced with graphic line buffer. Now the cursor is checked on every line.
  - Fixed REU handling: when REU is performing DMA bit 6,7 of D506 set which bank is involved.
  - Removed MMU registers from c64 mode: they are not visible.
- BeamRacer
  - Fixed deactivation, when writing to register 0x2E

### What's new in 1.7.0_b15 (Jan 22th 2021)
- General
	- Added drives 10 and 11: 8 to 11 are the available disk drives. The old local (not true drive) 10 has been moved to 12.
	- Removed drive's progress bar to optimize GUI space
	- New local filesystem mode: the user can attach a local directory to a disk (8 - 11). The files will be loaded into a virtual D64 (if there's space for all) that will be written back to the source directory when closed.
	- Added --prg-as-disk option: if checked every PRG file will be loaded as if it was a disk
	- Added --fullscreen --vdc-fullscreen options
	- Added --ignore-config-file option: if activated the emulator will not load any configuration file at start up.
	- Added Single Frame Mode for VIC & VDC : when in Single Frame Mode the user can advance in the emulation frame by frame.
	- Added cassette's controls over cassette's panel. Added forward & rewind controls.
	- Added Reset (CTRL+ALT+R) command that restores the last PRG file 
	- Added Hard Reset
	- Fixed LAX_I constant to 0xEE always
	- Fixed handling of G64 disk: now it's possible to write on empty tracks
	- Fixed CRT Comal80
	- Fixed ULTIMAX mode: writings to roms don't affect underlying ram.
	- Fixed testcart when screeshot is requested: must wait at least 1 frame. When IO is not active now testcart is disabled.
	- Fixed CPU 6510: must set I on reset
	- Added display effects: mirroring and flipping
	- Added tape counter
	- Improved tap handling: user can select in the preview panel which entry to run. Added tap counter on GUI
	- Modified border handling: removed check on cycle 63 in checkVertical
	- Added under various VIC menù the possibility to hide VIC's borders
	- VIC's new colodor Palette
	- Fixed SID's mute setting: after reset mute setting is preserved
	- Added support for NTSC
	- New brand Assembler (inspired by KickAssembler) with breakpoints, macros, high level statements, etc. see https://github.com/abbruzze/kernal64/tree/master/Kernal64/asm
	- Added EasyFlash support for flashing roms. Added EasyFlash jumper on GUI settings.
	- Added ROM reloading to change rom content without restarting emulator.
	- Added Settings panel on Help menu.
	- Added Cart Info panel on Cartridge menu.
	- Added new options for roms: --kernal, --charrom, --basic, etc.
	- Changed process exit code when a bad line argument is encountered: from 1 to 100 (for testbench)
- C64
	- Support for BeamRacer card (https://beamracer.net)
    ![](https://github.com/abbruzze/kernal64/blob/master/images/BeamRacer/bitmap.gif)
    ![](https://github.com/abbruzze/kernal64/blob/master/images/BeamRacer/reflections.gif)
- C128
	- Fixed function rom handling: if rom's size < 16K the mid rom is copied in the high rom. Now TurboAssembler128 works.
	- VDC: added on the VDC window the possibility to handle or not the automatic resolution adaptation of the monitor. Needed to run properly VDC-MCD demo (https://csdb.dk/release/?id=197895)
	- VDC: added handling of reg 27 > 0 to attributes processing in case of bitmap mode
	- Fixed testcart: when IO is not active now testcart is disabled

 

### What's new in 1.6.2_b8 (Oct 19th 2020) (update)
- C128
  - VDC: Fixed handling of rowCounterY & currentCharScanLine. Removed 0x1F anding. Now Volley For Two https://kollektivet.nu/v42 runs properly.
  - Fixed C128 48/80 & CAPS-LOCK key handling.
- General
  - Fixed problem with disk inserting introduced on 1.6.2_b5. Some demos stop working when it comes to wait disk changing

### What's new in 1.6.2_b5 (Sep 10th 2020) (update)
- General
  - added CSDB service provider. New entry under Games menù ![CSDB](https://github.com/abbruzze/kernal64/blob/master/images/csdb.png)
  - general keyboard handling improvements: added support for italian, german and english layout. Now the emulator will start with a default keyboard settings according to local layout.
  - Fixed a bug on keyboard editor: when the user changed a key mapping it takes place immediately, but if the user re-opens the editor the new mapping was lost.
  - Added state saving support for CRT.
  - Fixed keyboard control port: it didn't recognize key mapping modification. Restart was needed.
  - Now TCPRS232 has the same configuration string of Telnet one, so it can be possible to specify host:port later in order to use at commands.
  - Fixed error when keyboard layout (according to locale) is not found
- 1541/1571 drives
  - Fixed handling of acknowledge data line when DDR is not set to output. Now NeoHabitat (https://github.com/frandallfarmer/neohabitat/blob/master/README.md) works properly.
- C128
  - Added grey dot emulation for 8565 C128 only.

### What's new in 1.6.1 SCPU (Jun 4th 2020)
- CMD SuperCPU support for C64
  - Added new kscpu64 scripts to run the emulator with SCPU attached
  - Emulation of WD65816 is not cycle exact: most of SCPU software run properly except some timing accurate demos
  - Emulation of BA signal from VIC partially supported
  - Emulation of SIMM memory: up to 16M
  - Emulation of 1Mhz mode during I/O access partially supported
  - Shipped with VICE's SCPU rom. CMD's rom 2.04 also supported
  - New leds on the bottom: native (65816 in native mode), 20Mhz (turbo mode on), clickable JIFFYDOS (jiffy dos enabled), clickable SPEED (master speed selector), SIMM usage (% of SIMM usage)
  - Added command line options to configure simm size and jiffydos
  
![Metal Dust](https://github.com/abbruzze/kernal64/blob/master/images/scpu_metal.png)
![Wolf3D](https://github.com/abbruzze/kernal64/blob/master/images/scpu_wolf3d.png)

- General
  - Fixed cartridge type 5 banks handling
  - Fixed a bug when the user change the drive type 
  - New GIF Recorder utility: now you can create an animated GIF from what you see on the emulator (ALT-F)
  - Fixed CP/M cart with SCPU
  - Fixed cart type 19
  - When user load a saved state, the drivers' type are automatically set according to what it's stored on state.
  - Added --trace option to start emulation in trace mode
- 128
  - Fixed internal/external ROM configuration saving
  - Fixed bug on Z80 IRQ handling introduced during refactoring that prevents CP/M keyboard to work properly
  - Fixed handling of double char mode. Now Bucket Wars (Dual) 1.3 +2 ONSLAUGHT intro works fine


### What's new in 1.6.0 (Apr 3rd 2020)
- General
   - General code refactoring
   - GeoRAM cart supported: added --geo-ram command line option
   - PageFox cart supported
   - New implementation of CIAs
   - Added --cia-model command line option in order to choose new or old CIA
   - VIA implementation fixed
   - SID emulation: added --sid-cycle-exact command line option (see also in GUI's settings) in order to improve the SID emulation
   - Debugger: added cycle step by step debugging
   - CPU: fixed illegals ANE,SHS,SHY,SHX,SHA for RDY signal and ADC in decimal mode
   - Mouse : now the mouse emulation takes into account the delta T in order to slow down the movement
   - Drive emulation: 42 track D64 now supported
   - Added --cpujam-continue command line option for testbench
   - Added --screen-dim command line option for testbench
   - Added --screenshot command line option for testbench
- Testbench
  - Added support for Vice's testbench (see testbench directory)
- C128
  - registers 34 & 35 now are implemented
  - --vdc-enabled command line option changed in --vdc-disabled
  - Fixed handling of diverted pages in case of 256K
  - VDC
     - removed slider to select video size. Now the video resize is automatic!
     - Fixed char displaying when visible width < char width
     - Added xscroll table that following instructions of 128 Programmers Reference Guide : now Soci test 9 is almost there
     - Halved line cycles per row in case of interlace & text mode
     - Fixed calculation of total screen height (missing a + 1 for regs(4))
     - Fixed caching of chars & attr: added 1 position (for scrolling)
     - Added bound check for bitmap array to avoid ArrayIndexOutOfBoundException when screen height is adjusting
     - Introduced graphics & attributes buffers per row.
     -  emulated the maximum number of chars displayable: 82 chars. For positions > 82 the chars starting from 42th position are corrupted
     - Improved busy-flag emulation
     - Fixed attribute offset on reg(9) = 0
     - Little adjustment to busyFlagClearCycle delay in case of writing on reg(31)
     - On copyfill operation now the delay depends on copy or fill operation
     - Added handling of forced vsync if vsync is out of screen
     - CurrentCharScanLine & rowCounterY are now limited to 31
     - Fixed restoring of VDC state when interlace mode was selected   
     - New vertical handling: now VDC101 is fully supported

### What's new in 1.5.3 (Nov 22th 2019)
- Fixed bug that prevents G64 floppy to run properly on 1571 drive.
- New 2.13 Scala libraries.
- Cart rom handling fix for unordered rom's index sequence
- New command line option: --cpm64-enabled
- Fixed VDC color palette
- Fixed sprite handling for "Krestage 3" 50 pixels wide sprites
- When JAM opcodes are executed the user can choose if continue or open debugger
- Improved disk change recognition: Now the R/W disk head will read an empty disk for a while while user is changing disk
- New command line option: --run-file. Can be used to run the specific file (on the attached disk) at startup
- Fixed track allocation table for .D64 modified for .D71 support.
- Removed disk insertion emulation when user is dragging a disk
- D64: added last sector gap
- Added directory/bam support for G64 format: now G64 can be previewed
- Minor fixes to pass more CIA's tests
- Added support for Zaxxon & Mach 5 carts
- VDC: New screen size selection from VDC main screen. Useful to display images larger than PAL dimensions.
- CPU: Fixed RTI, CLI, SEI and PLP; RTI: executes irq immediately if after restoring flags I = 0 and irq is pending; CLI,SEI,PLP: execute next instruction even if irq condition is satisfied
- VIC: general improvements for sprites and gfx

### What's new in 1.5.2 (May 30th 2019)
- CIA major bug fixed that prevented ComaLand to work properly
- Color memory now handles the most significant nibble as the last nibble written by VIC: dadb (openio) test now works properly.
- Fixed sprite idle cycle read.
- Now it's possible to choose among 3 differente VIC palette
- Fixed a veeery old bug about irq/nmi timings. The bug caused some irq stable interrupt handling to malfunctioning cause of an additional random cycle. In some demo the bug is visible when an unexpected flickering appears.
- Normal Mode -> Warp Mode -> Normal Mode fixed. Fixed warp mode handling causing a delay to SID output. When resetting emulator or using warp mode and then normal mode SID emits output with a short initial delay.

### What's new in 1.5.1 (May 4th 2019)
- VIC: Improved invalid video modes.
- Fixed Cart type 5 (Ocean) for Vice test
- Dual SID: more addresses to choose from for second SID.
- C128 MMU: fixed page diverting issue when common RAM is enabled on bottom position.
- General minor bugs fixed

### What's new in 1.5.0 (Feb 3rd 2019)
- VIC: fixed den bit handling: now on $30 it's checked on every cycles
- VIC: fixed sprite idle cycle, now display internal bus
- REU: fixed transfer timings.
- Now, demo Bottom (REU) works fine (https://csdb.dk/release/?id=174066). Fixed also Treu Love, now works fine for 80% (https://csdb.dk/release/?id=144105)
- Game pad controller: changed thresholds, now PS4 gamepad works properly

### What's new in 1.4.9 (Dec 9th 2018)
- VICII 2Mhz mode partial support added. Now Risen From Oblivion for VIC has been improved and the moving image is shown. Cannot show the first image yet.
- New ROMs panel. Removed rom environment variable. Now the roms configuration can be saved as other settings.
- New drives settings panel. Now it's possible to change drives settings and load floppy from a floating window. Removed all drive settings from menu'
-  The keyboard layout can be changed and the user can take effects immediately without restart.
- New warp mode on loading
- New RS-232 handling. New GUI panel to configure rs-232 and to monitor signals. RS-232 general improvements. New Hayes Modem class to handle AT commands.
- Z80 cycle counting more accurate
- Added VDC interlace mode user option. Now the user can choose between normal interlace mode (with flickering) and automatic de-interlace mode (default).

### What's new in 1.4.8 (Oct 20th 2018)
**VDC improvements**
- Improved screen resolution and aspect ratio
- Different handling of borders: seems better than before but need more investigation
- Added handling of 16K/64K bit
- Fixed interlace bitmap color mode: seems a lot better than before. For sure need more investigation.
- Fixed double pixel mode in bitmap mode
- Added busy flag emulation on copy/fill operations
- HBlank is not yet implemented

**General**
- Improved cart button functionality for freezers: now the freezer will activate interrupt only if the CPU is in the fetching state.
- VIC BA requests skipped when 2Mhz mode is on
- Improved 1581 speed cycles in order to run fast loaders in C64 mode properly
- All disks now are write protected if the corresponding file on local drive is read-only
- New scala 2.12.7 libraries
- Changed keyboard handling of non-standard keys. Used extended key codes to handle non standard keys. Now the keyboard file layout is simpler. Old keyboard file layout does not work any more.

### What's new in 1.4.7 (Sep 10th 2018)

**Lots of improvements in VDC handling.**

- fixed 8x1 mode
- fixed monochrome interlaced mode
- fixed clocks needed to draw a line depending on several parameters and
not only by xchars_total
- improved x smooth handling
- most of the Soci's test run properly
- regs 34 & 35 not yet implemented, need more investigation
- added customization of VDC total scan line: the user can enter the
total number of CRT scan line instead of having 312 PAL lines. New menù under Settings -> Adjust display -> VDC scan lines
- VDC was not included in the MMU in C64 mode, so in C64 mode it was not possible to see VDC running
- light pen support added (can't be used with light pen mouse emulation)

**General**
- 2Mhz mode has been improved: the VIC refresh cycles are treated as I/O.
- minor bug fixing

### What's new in 1.4.6 (Aug 10th 2018)
- Minor bug fixing. Added more command line options (see them with --help). Fixed REU 128K.
- Added support for right shift key
- Rotation calibrated with a correction factor of 1.025: Now the rpm is about 300 and most of the non running demos now run properly
- Added test cart to execute test bench (Writing at $D7FF will force the emulator to exit with the given exit code)
- Keyboard Editor: fixed TAB key handling
- REU fixed minor bugs about FF00 management. Fixed 128K size broken reu.
- Digimax now the user can choose which is the sampling frequency. Now modplay runs properly (used 11K)
- 128 MMU fixed page1 pointer at startup
- added k64s.bat & k128s.bat to run emulator with shell support on Windows

### What's new in 1.4.5 (Jul 25th 2018)
- Added settings saving: now you can save the (majority) of the current settings
- Added support for function rom "Megabit" (C128 only)
- Fixed minor bugs that prevented the restoring of main window's size
- Added new menus for display settings (aspect ratio, zoom, rendering)
- Modified the display rendering type in order to improve image quality
- Added command line options (--help does work on Linux based system, on Windows the kxxx.bat script must be modified to see the usage)
- Added new utility under Edit menù: List BASIC to editor. List the BASIC program stored in memory to the default external editor
- Added the Eject disk menù under File
- VDC window is now resizable

Thanks to Mirkosoft for testing.

### What's new in 1.4.4 (May 1st 2018)
* Added DigiMax UserPort support
* Added GMOD2 cart support: new menu (Settings -> IO -> Gmod2 eeprom file ...) for eeprom state savings.

Sam's Journey snapshot:

![](https://github.com/abbruzze/kernal64/blob/master/images/sam_journey.png)

### What's new in 1.4.3 (Mar 3rd 2018)
* Added 1581 disk drive support (MFM format)
* Added new menù (under Settings) to select drives type (1541/71/81)
* Added "write changes on disk" flag under File menù: when the flag is off the disk changes will not be stored on local disk

### What's new in 1.4.2 (Feb 2nd 2018)
* Added 1571 disk drive support (MFM format will be ready for 1581)
* Removed Dropbox drive

### What's new in 1.4.1 (Jan 4th 2018)
* New mouse handling: now the o.s. mouse hiding works properly and the mouse pointer cannot escape from the emulator

### What's new in 1.4.0 (Jul 14th 2017)
* New Commodore 128 emulator
* General refactoring and minor bug fixing
* Fix of severe bug on CIA when timer B counts timer A
* Added keyboard layout editor in order to customize keyboard mapping
* New scripts for launching emulators
* Scala 2.12 & jre 1.8

Main c128 emulator's features:
* Z80 and CP/M
* 2Mhz mode (still not cycle exact)
* new keys and keypad
* VDC support: interlace mode supported (known issues: 8x1 mode supported but still not working properly, interlaced color mode has some minor visualization problems)
* VICii (interlaced mode not supported)
* 1571 drive not yet implemented (JiffyDOS for 128 works fine)
* MMU panel to check Z80, C64 mode and 2Mhz use

<img src="https://github.com/abbruzze/kernal64/blob/master/images/c128_cpm.jpg" width="600" height="454" alt="Commodore 128 CP/M 3.0"/>
<img src="https://github.com/abbruzze/kernal64/blob/master/images/c128_interlaced.jpg" width="600" height="454" alt="Commodore 128 interlaced mono"/>
<img src="https://github.com/abbruzze/kernal64/blob/master/images/c128_rfo.jpg" width="600" height="454" alt="Commodore 128 Risen from oblivion demo"/>

### What's new in 1.3.1 (Dic 2nd 2016)
* RS-232 improvements. Added baud rate in the configuration parameters
* New ZIP format: now you can open directly zip files containing C64 files, like D64, PRG, CRT ...
* New "game jukebox" feature: a new plug & play components used to collect games, demos, intros, etc. from internet and play with them easily and immetiately. Out of the box you can find Gamebase (http://www.gb64.com/) component and Pouet demo (http://www.pouet.net/)  component.

<img src="https://github.com/abbruzze/kernal64/blob/master/images/pouet.jpg" width="724" height="427" alt="Pouet demos"/>
<img src="https://github.com/abbruzze/kernal64/blob/master/images/gamebase.jpg" width="724" height="427" alt="Gamebase"/>

### What's new in 1.3.0 (Sep 23th 2016)
* Fixed VIC xcoord bug: now sprites can be viewed properly on sideborders
* Fixed IRQ delay cycles that caused image flickering
* Fixed display size
* New feature: now it ca be possible to save and reload the emulator's state.

### What's new in 1.2.7 (Aug 15th 2016)
* Improved CPU & VIC timings. Now nufli images can be displayed properly as well as nuvie videos.

### What's new in 1.2.6 (May 26th 2016)
* Remoting (experimental) : share the emulator's screen with your friend on the network to play games in two-player mode
* Removed jiffydos env variable: now the user must provide the JiffyDOS ROMS

### What's new in 1.2.5 (May 5th 2016)
* Added support for [Flyer](http://www.retroswitch.com/products/flyer/) internet modem (see Wiki)
* Minor RS-232 improvements and fixes

### What's new in 1.2.4 (Apr 1st 2016)
* Added support for stereo dual SID chip

### What's new in 1.2.3 (Dic 6th 2015)
* Added support for EasyFlash cart: eprom flashing is not emulated

### What's new in 1.2.2
* Added support for dual drive (device 9)
* Local driver and Dropbox support moved to device 10 
* Added D64 extended format with errors

### What's new in 1.2.1
* Added support for **CP/M** cartridge (see wiki for details)

### A Scala Commodore 64 & 128 emulator

Written in the Scala language (http://www.scala-lang.org/) and available for any Java JRE 1.8 environment.

Here the main features:
* Cycle based: exact cycle emulation using the PAL frequency
* VIC emulation based on the Christian Bauer's article: "The MOS 6567/6569 video controller (VIC-II) and its application in the Commodore 64" and the VIC-Addendum.
* 6510 CPU full emulation (with illegal opcodes too). The user can choose between a cycle exact cpu and a faster cpu not cycle exact.
* CIA1, CIA2 chips emulation: almost full.
* IEC Serial Bus
* Keyboard: for key mapping I'll provide documentation
* SID chip: for this chip I used the RSID Java library by Ken Händel
* Joystick emulation for port 1 and 2: the joystick can be emulated via keypad or via an USB real joystick (thanks to jinput library)
* Commodore 1351 mouse emulation
* Light pen emulation
* Datassette: full emulation using TAP file (read/write)
* 1541 Drive: exact cycle emulation (read/write) using 6502 CPU (1Mhz). Supports D64 and G64. In the Settings menù it's possible to turn off the full drive emulation and turn on the faster one.
  Only the G64 format can be formatted by Dos, while for the D64, the emulator intercepts the format routine call and bypasses it, using a pre-formatted empty disk.
* Local drive emulation on device 10: choose a local directory and let it your drive 10.
* **Dropbox** drive emulation on device 10: connect your Dropbox account to Kernal64 and let access it using device 10!
* Cartridges emulation (some CRT types are not emulated). Supports CRT format.
* MPS803 printer emulation. Preview panel updated while printing.
* Fast program loading of PRG/T64 local file or D64's file entry.
* Debug panel for main CPU and for 1541's cpu (break point, step by step execution, disassembler, assembler, etc.)
* Component panel: shows all internal components while running
* Drag & Drop support
* REU support (128,256,512,16M)
* Support for external roms, both for C1541 kernal and C64 kernal. The roms must be put in the roms directory. Use the switches -Dkernal=rom name and -D1541_kernal=rom name (you can put JiffyDOS ROMS here, for example, to use the fastloader).
* Support for 1541-VIA1 <-> CIA2 parallel cable, used by many fastloaders. Tested on Speed Dos and Dolphin Dos 2.
* Support for 1541's expanded memory (tested with Dolphin Dos 2).
* RS-232 3-way UserPort implementations useful to connect with BBS on internet. The Telnet implementation can be used to connect to a telnet server (like BBSs); the TCP implementation can be used to connect with a generic TCP/IP server. The File implementation can be used to read/write to local files.
* RS-232 **SwiftLink** cartridge implementation. Tried with NovaTerm 9.6 and other terminal software. 
* ... and more
