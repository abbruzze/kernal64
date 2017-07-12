@echo off
set HOME=%~dp0
set LIB=%HOME%lib
set CP=%LIB%\kernal64.jar;%LIB%\scala-library.jar
java -Xms32M -Xmx32M -cp %CP% ucesoft.cbm.remote.RemoteC64Client %*
