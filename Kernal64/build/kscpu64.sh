#!/bin/bash

HOME=`dirname "$0"`
HOME=`cd "$HOME"; pwd -P`
LIB=$HOME/lib
CP=
if [ ! -x $JAVA_HOME/bin/java ]; then
        JAVA=java
else
        JAVA=$JAVA_HOME/bin/java
fi
$JAVA -server -Xms64M -Xmx256M -cp $CP -Djava.library.path=$LIB ucesoft.cbm.scpu.SCPUC64 "$@"
