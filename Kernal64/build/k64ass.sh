#!/bin/bash

HOME=`dirname "$0"`
HOME=`cd "$HOME"; pwd -P`
LIB=$HOME/lib
CP=$LIB/kernal64.jar:$LIB/scala-library.jar:$LIB/scala-parser-combinators_2.13-1.1.2.jar
if [ ! -x $JAVA_HOME/bin/java ]; then
        JAVA=java
else
        JAVA=$JAVA_HOME/bin/java
fi
$JAVA -cp $CP ucesoft.cbm.cpu.asm.Assembler "$@"
