#!/bin/sh

if [ -z $1 ]; then LISTEN_PORT=50522; else LISTEN_PORT=$1; fi

clear

nc -l -p $LISTEN_PORT
exit 0

