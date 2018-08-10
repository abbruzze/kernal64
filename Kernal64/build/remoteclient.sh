#!/bin/bash

HOME=`dirname "$0"`
HOME=`cd "$HOME"; pwd -P`
LIB=$HOME/lib
CP=$ROMS:$LIB/kernal64.jar:$LIB/scala-library.jar
if [ ! -x $JAVA_HOME/bin/java ]; then
        JAVA=java
else
        JAVA=$JAVA_HOME/bin/java
fi
$JAVA -Xms32M -Xmx32M -cp $CP ucesoft.cbm.remote.RemoteC64Client $*
