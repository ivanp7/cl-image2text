#!/bin/sh

if [ -z $1 ]; then SERVER_PORT=50511; else SERVER_PORT=$1; fi
if [ -z $2 ]; then SERVER_IP=127.0.0.1; else SERVER_IP=$2; fi
if [ -z $3 ]; then REDIRECT_PORT=50522; else REDIRECT_PORT=$3; fi

if ! ss -ln | grep -c $REDIRECT_PORT > /dev/null
then 
    echo 'Start client-output.sh in the other window first!'
    exit 1
fi

clear

while read -s -n1 c
do echo "$c"
done | nc $SERVER_IP $SERVER_PORT | nc 127.0.0.1 $REDIRECT_PORT

