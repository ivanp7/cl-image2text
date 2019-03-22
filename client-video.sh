#!/bin/sh

XMIN=$1
YMIN=$2
XMAX=$3
YMAX=$4

if [ -z $5 ]; then SERVER_PORT=50511; else SERVER_PORT=$1; fi
if [ -z $6 ]; then SERVER_IP=127.0.0.1; else SERVER_IP=$2; fi

{
    echo "video"
    echo "$XMIN $YMIN $XMAX $YMAX";
    while read -s -n1 c
    do echo "$c"
    done;
} | nc $SERVER_IP $SERVER_PORT

