@echo off
set HOME=%~dp0
set LIB=%HOME%lib
set CP=%LIB%\kernal64.jar;%LIB%\jinput.jar;%LIB%\scala-library.jar;%LIB%\scala-parser-combinators_2.13-1.1.2.jar;%LIB%\commons-net-3.3.jar;%LIB%\jsoup-1.13.1.jar;%LIB%\rsyntaxtextarea-3.1.1.jar
java -server -Xms64M -Xmx256M -cp %CP% -Djava.library.path=%LIB% ucesoft.cbm.scpu.SCPUC64 %*
