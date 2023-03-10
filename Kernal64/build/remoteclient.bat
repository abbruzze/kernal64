@echo off
set HOME=%~dp0
set LIB="%HOME%lib"
set CP=
java -Xms32M -Xmx32M -cp %CP% ucesoft.cbm.remote.RemoteC64Client %*
