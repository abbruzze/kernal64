@echo off
set HOME=%~dp0
set LIB=%HOME%lib
set ROMS=%HOME%roms
set CP=%ROMS%;%LIB%\kernal64.jar;%LIB%\jinput.jar;%LIB%\scala-library.jar;%LIB%\scala-parser-combinators_2.11-1.0.3.jar;%LIB%\commons-net-3.3.jar;%LIB%\dropbox-core-sdk-1.7.7.jar;%LIB%\jackson-core-2.2.4.jar
start javaw -server -Xms64M -Xmx64M -cp %CP% -Djava.library.path=%LIB% ucesoft.c64.C64
