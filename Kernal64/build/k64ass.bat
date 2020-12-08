@echo off
set HOME=%~dp0
set LIB=%HOME%lib
set CP=%LIB%\kernal64.jar;%LIB%\scala-library.jar;%LIB%\scala-parser-combinators_2.13-1.1.2.jar
java -cp %CP% ucesoft.cbm.cpu.asm.Assembler %*
