@echo off
set HOME=%~dp0
set LIB="%HOME%lib"
set CP=
set CLASS=ucesoft.cbm.vic20.VIC20
if "%1" == "--shell" goto shift_shell
if "%1" == "--help" goto help_shell
start javaw -server -Xms64M -Xmx128M -cp %CP% -Djava.library.path=%LIB% %CLASS% %*
goto end

:shift_shell

set Args=
:Parse
shift
set First=%1
if not defined First goto :EndParse
  set Args=%Args% %First%
  goto :Parse
:EndParse
goto shell

:help_shell
set Args=%*

:shell
java -server -Xms64M -Xmx128M -cp %CP% -Djava.library.path=%LIB% %CLASS% %Args%
:end