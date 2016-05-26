@echo off
set HOME=%~dp0
set LIB=%HOME%lib
set CP=%LIB%\kernal64.jar;%LIB%\scala-library.jar
java -Xms8M -Xmx8M -cp %CP% ucesoft.c64.remote.RemoteC64Client %*