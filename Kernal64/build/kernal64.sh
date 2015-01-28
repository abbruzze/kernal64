#!/bin/bash

HOME=`dirname "$0"`
HOME=`cd "$HOME"; pwd -P`
LIB=$HOME/lib
CP=$LIB/kernal64.jar:$LIB/jinput.jar:$LIB/scala-library.jar
if [ ! -x $JAVA_HOME/bin/java ]; then
        JAVA=java
else
        JAVA=$JAVA_HOME/bin/java
fi

$JAVA -server -Xms64M -Xmx64M -cp $CP -Djava.library.path=$LIB ucesoft.c64.C64
