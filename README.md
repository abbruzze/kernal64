[![Build Status](https://travis-ci.org/abbruzze/kernal64.svg?branch=master)](https://travis-ci.org/abbruzze/kernal64)

Kernal64 ver 1.5.1
========
![](https://github.com/abbruzze/kernal64/blob/master/images/c64.jpg)![](https://github.com/abbruzze/kernal64/blob/master/images/c128.jpg)
<img src="https://github.com/abbruzze/kernal64/blob/master/images/commodore128_vdc.jpg" alt="Commodore 128 VDC"/>

### Installation
Go to https://github.com/abbruzze/kernal64/releases/latest and download and unzip on your computer the latest version.
Be sure to have a jre (1.8 or above) in the path and launch the **k64.bat** or **k64.sh** or **k128** equivalent for 128 emulator batch file, depending on your operating system.
I experienced a boost in performance using Java 9+.

Windows users that want shell support (for --help command line, for example) could use the **k64s.bat** or **k128s.bat** scripts.

The scripts start java with the -server option for performance reason. It could happen that your Java installation does not support the server option. In this case edit the script and remove the -server option.

If you want to load a 16M REU modify the memory settings inside the start scripts: for example, you could set -Xmx128M.

### Wiki
Wiki pages are available here: https://github.com/abbruzze/kernal64/wiki

### What's new in 1.5.1 (May 4th 2019)
- VIC: Improved invalid video modes.
- Fixed Cart type 5 (Ocean) for Vice test
- Dual SID: more addresses to choose from for second SID.
- C128 MMU: fixed page diverting issue when common RAM is enabled on bottom position.
- General minor bugs fixed
- CIA major bug fixed (introduced on the 1st 128 release) that prevented ComaLand to work properly
- Color memory now handles the most significant nibble as the last nibble written by VIC: dadb (openio) test now works properly.
- Fixed sprite idle cycle read.
- Now it's possible to choose among 3 differente VIC palette

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
