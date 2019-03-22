#!/bin/sh

if [ -z $1 ]; then SIZE_X=318; else SIZE_X=$1; fi
if [ -z $2 ]; then SIZE_Y=81; else SIZE_Y=$2; fi

if [ -z $3 ]; then SERVER_PORT=50511; else SERVER_PORT=$1; fi
if [ -z $4 ]; then SERVER_IP=127.0.0.1; else SERVER_IP=$2; fi

{
    echo "master"
    echo "$SIZE_X $SIZE_Y";
    while read -s -n1 c
    do echo "$c"
    done;
} | nc $SERVER_IP $SERVER_PORT

