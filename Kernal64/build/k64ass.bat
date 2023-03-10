@echo off
set HOME=%~dp0
set LIB="%HOME%lib"
set CP=
java -cp %CP% ucesoft.cbm.cpu.asm.Assembler %*
