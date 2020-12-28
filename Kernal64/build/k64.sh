#!/bin/bash

HOME=`dirname "$0"`
HOME=`cd "$HOME"; pwd -P`
LIB=$HOME/lib
CP=$LIB/kernal64.jar:$LIB/jinput.jar:$LIB/scala-library.jar:$LIB/scala-parser-combinators_2.13-1.1.2.jar:$LIB/commons-net-3.3.jar:$LIB/jsoup-1.13.1.jar:$LIB/rsyntaxtextarea-3.1.1.jar
if [ ! -x $JAVA_HOME/bin/java ]; then
        JAVA=java
else
        JAVA=$JAVA_HOME/bin/java
fi
$JAVA -server -Xms64M -Xmx128M -cp $CP -Djava.library.path=$LIB ucesoft.cbm.c64.C64 $*
