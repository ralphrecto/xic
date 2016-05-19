#!/bin/sh
#
# This is a very simple script that uses gcc to link in a given .s
# file to the xi runtime library, and uses xifilt to help 
# decode error messages
#
# Use this like ./linkxi.sh -o binary foo.s
#
DIR=$(dirname $0)
ABI_FLAG=$($DIR/platform-flags.sh)

# echo "ABI_FLAG = $ABI_FLAG"

gcc $ABI_FLAG "$@" -L$DIR -lxi -lpthread 2>&1 | $DIR/xifilt
