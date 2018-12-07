@echo off
set HOME=%~dp0
set LIB=%HOME%lib
set CP=%LIB%\kernal64.jar;%LIB%\jinput.jar;%LIB%\scala-library.jar;%LIB%\scala-parser-combinators_2.12-1.0.5.jar;%LIB%\commons-net-3.3.jar
java -server -Xms64M -Xmx128M -cp %CP% -Djava.library.path=%LIB% ucesoft.cbm.c128.C128 %*
