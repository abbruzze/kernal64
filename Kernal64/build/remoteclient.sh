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
$JAVA -Xms8M -Xmx8M -cp $CP ucesoft.c64.remote.RemoteC64Client $*