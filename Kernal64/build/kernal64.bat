@echo off
set HOME=%~dp0
set LIB=%HOME%lib
set CP=%LIB%\kernal64.jar;%LIB%\jinput.jar;%LIB%\scala-library.jar
start javaw -server -Xms64M -Xmx64M -cp %CP% -Djava.library.path=%LIB% ucesoft.c64.C64
