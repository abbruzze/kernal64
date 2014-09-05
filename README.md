kernal64
========
![](https://github.com/abbruzze/kernal64/blob/master/images/c64.jpg)

###An almost full Scala Commodore 64 emulator

Written in the Scala language (http://www.scala-lang.org/) and available for any Java JRE 1.7 environment.

Here the main features:
* Cycle based: exact cycle emulation using the PAL frequency
* VIC emulation based on the Christian Bauer's article: "The MOS 6567/6569 video controller (VIC-II) and its application in the Commodore 64". I know that it's incomplete (and in some cases buggy). I tried to close some issues by myself. Some others,Sprite Crunch for instance, are still open
* 6502 CPU full emulation (with illegal opcodes too)
* CIA1, CIA2 chips emulation: almost full. RS-232 lines are not emulated
* IEC Serial Bus
* Keyboard: for key mapping I'll provide documentation
* SID chip: for this chip I used the RSID Java library by Ken Händel
* Joystick emulation for port 1 and 2: the joystick can be emulated via keypad or via an USB real joystick (thanks to jinput library)
* Commodore 1351 mouse emulation
* Light pen emulation
* Datassette: full emulation using TAP file (read/write)
* 1541 Drive: exact cycle emulation (read/write) using 6510 CPU (1Mhz). Supports D64 format only. In the Settings menù it's possible to turn off the full drive emulation and turn on the faster one.
* Cartridges emulation (some CRT types are not emulated). Supports CRT format.
* Fast program loading of PRG/T64 local file or D64's file entry.
* Debug panel for main CPU and for 1541's cpu (break point, step by step execution, disassembler, assembler, etc.)
* Component panel: shows all internal components while running
* ... and more

In the meanwhile you can download the dist/kernal64_install.zip file to try it out: be sure to have Java installed.
