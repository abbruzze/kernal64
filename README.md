kernal64 ver 0.9.9O
========
![](https://github.com/abbruzze/kernal64/blob/master/images/c64.jpg)

###An almost full Scala Commodore 64 emulator

Written in the Scala language (http://www.scala-lang.org/) and available for any Java JRE 1.7 environment.

Here the main features:
* Cycle based: exact cycle emulation using the PAL frequency
* VIC emulation based on the Christian Bauer's article: "The MOS 6567/6569 video controller (VIC-II) and its application in the Commodore 64". I know that it's incomplete (and in some cases buggy). I tried to close some issues by myself. Some others,Sprite Crunch for instance, are still open. With the current implementation most of the games run properly. Some demos run with "strange" effects, naturally.
* 6510 CPU full emulation (with illegal opcodes too). The user can choose between a cycle exact cpu and a faster cpu not cycle exact.
* CIA1, CIA2 chips emulation: almost full.
* IEC Serial Bus
* Keyboard: for key mapping I'll provide documentation
* SID chip: for this chip I used the RSID Java library by Ken Händel
* Joystick emulation for port 1 and 2: the joystick can be emulated via keypad or via an USB real joystick (thanks to jinput library)
* Commodore 1351 mouse emulation
* Light pen emulation
* Datassette: full emulation using TAP file (read/write)
* 1541 Drive: exact cycle emulation (read/write) using 6502 CPU (1Mhz). Supports D64 format only. In the Settings menù it's possible to turn off the full drive emulation and turn on the faster one.
* Local drive emulation on device 9: choose a local directory and let it your drive 9.
* **Dropbox** drive emulation on device 9: connect your Dropbox account to Kernal64 and let access it using device 9!
* Cartridges emulation (some CRT types are not emulated). Supports CRT format.
* MPS803 printer emulation. Preview panel updated while printing.
* Fast program loading of PRG/T64 local file or D64's file entry.
* Debug panel for main CPU and for 1541's cpu (break point, step by step execution, disassembler, assembler, etc.)
* Component panel: shows all internal components while running
* Drag & Drop support
* REU support (128,256,512,16M)
* JiffyDOS support (use -Djiffydos environment variable)
* RS-232 3-way UserPort implementations useful to connect with BBS on internet. The Telnet implementation can be used to connect to a telnet server (like BBSs); the TCP implementation can be used to connect with a generic TCP/IP server. The File implementation can be used to read/write to local files.
* RS-232 **SwiftLink** cartridge implementation. Tried with NovaTerm 9.6 and other terminal software. 
* ... and more

###Installation
Download and unzip https://github.com/abbruzze/kernal64/tree/master/Kernal64/dist on your computer.
Be sure to have a jre (1.7 or above) in the path and launch the **kernal64.bat** or **kernal64.sh** batch file, depending on your operating system.

###Wiki
Wiki pages are available here: https://github.com/abbruzze/kernal64/wiki
